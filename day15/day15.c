#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

static int debug = 0;
static int debug_channel = 1;

// IntCode computer

typedef long opcode;
typedef opcode (* input_function)(void *);
typedef void (* output_function)(void *, opcode);

struct program {
	size_t memory_size;
	size_t pc;
	size_t relative_base;
	input_function input;
	output_function output;
	void *io_context;
	opcode *memory;
};

enum parameter_mode {
	position_mode = 0,
	immediate_mode = 1,
	relative_mode = 2
};

size_t memory_size(size_t size) {
	return size * sizeof(opcode);
}

size_t increase_memory(opcode **memory, size_t old_size) {
	size_t new_size = old_size*2;
	*memory = (opcode *)realloc(*memory, memory_size(new_size));
	memset(*memory + old_size, 0, memory_size(old_size));
	return new_size;
}

struct program *load_program(char *filename) {
	size_t size = 100;
	opcode *memory = (opcode *)malloc(memory_size(size));
	memset(memory, 0, memory_size(size));
	size_t pos = 0;
	opcode cur = 0;
	opcode sign = 1;
	FILE *f = fopen(filename, "rt");
	for (;;) {
		int c = fgetc(f);
		if (c == EOF || c == ',') {
			if (pos == size) {
				size = increase_memory(&memory, size);
			}
			memory[pos++] = cur * sign;
			cur = 0;
			sign = 1;
		} else if (c == '-') {
			sign = -1;
		} else if ('0' <= c && c <= '9') {
			cur = (cur * 10) + (c - '0');
		}
		if (c == EOF) break;
	}
	fclose(f);


	struct program *program = (struct program *)malloc(sizeof(struct program));
	program->memory_size = size;
	program->memory = memory;
	program->pc = 0;
	program->relative_base = 0;
	return program;
}

void free_program(struct program *program) {
	free(program->memory);
	free(program);
}

struct program *copy_program(struct program *program) {
	struct program *copy = (struct program *)malloc(sizeof(struct program));
	copy->memory_size = program->memory_size;
	copy->memory = (opcode *)malloc(memory_size(program->memory_size));
	memcpy(copy->memory, program->memory, memory_size(program->memory_size));
	copy->input = program->input;
	copy->output = program->output;
	copy->io_context = program->io_context;
	copy->pc = 0;
	copy->relative_base = 0;
	return copy;
}

void ensure_program_memory(struct program *program, size_t address) {
	while (address >= program->memory_size) {
		program->memory_size = increase_memory(&program->memory, program->memory_size);
	}
}

enum parameter_mode parameter_mode_from_opcode(opcode x, size_t index) {
	switch (x) {
		case 3:
			return position_mode;
		case 4:
			return position_mode;
		default:
			assert(index >= 1);
			x /= 100;
			while (--index) x /= 10;
			return x % 10;
	}
}

int read_opcode(struct program *program, opcode *x) {
	ensure_program_memory(program, program->pc);
	*x = program->memory[program->pc];
	return 0;
}

int read_parameter(struct program *program, size_t index, opcode *x) {
	ensure_program_memory(program, program->pc + index);

	enum parameter_mode mode = parameter_mode_from_opcode(program->memory[program->pc], index);
	opcode immediate = program->memory[program->pc + index];
	switch (mode) {
		case position_mode: {
			ensure_program_memory(program, immediate);
			if (debug > 10) {
				printf("read index %d position mode (immediate %ld) --> %ld", 
					index, immediate, program->memory[immediate]);
			}
			*x = program->memory[immediate];
			break;
		}
		case immediate_mode: {
			if (debug > 10) printf("read index %d immediate --> %ld\n", index, immediate);
			*x = immediate;
			break;
		}
		case relative_mode: {
			size_t address = program->relative_base + immediate;
			ensure_program_memory(program, address);
			if (debug > 10) {
				printf("read index %d relative (immediate %ld relative %u) --> %ld\n", 
					index, immediate, address, program->memory[address]);
			}
			*x = program->memory[address];
			break;
		}
	}

	return 0;
}

int write_program(struct program *program, size_t index, opcode value) {
	ensure_program_memory(program, program->pc + index);

	enum parameter_mode mode = parameter_mode_from_opcode(program->memory[program->pc], index);
	opcode immediate = program->memory[program->pc + index];
	switch (mode) {
		case immediate_mode:
			return 1;

		case position_mode: {
			ensure_program_memory(program, immediate);
			if (debug > 10) {
				printf("write index %d position mode (immediate %d) %d <-- %d\n",
					index, immediate, program->memory[immediate], value);
			}
			program->memory[immediate] = value;
			break;
		}
		case relative_mode: {
			size_t address = program->relative_base + immediate;
			ensure_program_memory(program, address);
			if (debug > 10) {
				printf("write index %d relative mode (immediate %d relative %d) <-- %ld\n",
					index, immediate, address, value);
			}
			program->memory[address] = value;
			break;
		}
	}

	return 0;
}

void print_program(struct program *program) {
	printf("%ld", program->memory[0]);
	for (int i = 1; i < program->memory_size / sizeof(opcode); ++i) {
		printf(",%ld", program->memory[i]);
	}
	printf("\n");
}

int program_has_halted(struct program *program) {
	opcode x;
	if (read_opcode(program, &x) != 0) return 1;
	return x == 99;
}

void debug1(struct program *program, opcode i, char *instr, opcode x) {
	if (debug) {
		printf("%04u %16d %4s %d\n", program->pc, i, instr, x);
	}
}

void debug2(struct program *program, opcode i, char *instr, opcode x, opcode y) {
	if (debug) {
		printf("%04u %16d %4s %d,%d\n", program->pc, i, instr, x, y);
	}
}

int run_program(struct program *program, int cooperative) {
	opcode i;
	while (read_opcode(program, &i) == 0 && i != 99) {
		switch (i % 100) {
			case 1: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 1001;
				if (read_parameter(program, 2, &y) != 0) return 1002;
				debug2(program, i, "add", x, y);
				if (write_program(program, 3, x + y) != 0) return 1004;
				program->pc += 4;
				break;
			}
			case 2: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 2001;
				if (read_parameter(program, 2, &y) != 0) return 2002;
				debug2(program, i, "mul", x, y);
				if (write_program(program, 3, x * y) != 0) return 2004;
				program->pc += 4;
				break;
			}
			case 3: {
				opcode x = program->input(program->io_context);
				debug1(program, i, "in", x);
				if (write_program(program, 1, x) != 0) return 3004;
				program->pc += 2;
				break;
			}
			case 4: {
				opcode x;
				if (read_parameter(program, 1, &x) != 0) return 4001;
				debug1(program, i, "out", x);
				program->output(program->io_context, x);
				program->pc += 2;
				if (cooperative)
					return 0;
				else
					break;
			}
			case 5: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 5001;
				if (read_parameter(program, 2, &y) != 0) return 5002;
				debug2(program, i, "jnz", x, y);
				program->pc = (x != 0) ? y : program->pc + 3;
				break;
			}
			case 6: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 6001;
				if (read_parameter(program, 2, &y) != 0) return 6002;
				debug2(program, i, "jz", x, y);
				program->pc = (x == 0) ? y : program->pc + 3;
				break;
			}
			case 7: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 7001;
				if (read_parameter(program, 2, &y) != 0) return 7002;
				debug2(program, i, "lt", x, y);
				if (write_program(program, 3, (x < y) ? 1 : 0) != 0) return 7004;
				program->pc += 4;
				break;
			}
			case 8: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 8001;
				if (read_parameter(program, 2, &y) != 0) return 8002;
				debug2(program, i, "eq", x, y);
				if (write_program(program, 3, (x == y) ? 1 : 0) != 0) return 8004;
				program->pc += 4;
				break;
			}
			case 9: {
				opcode x;
				if (read_parameter(program, 1, &x) != 0) return 9001;
				debug1(program, i, "base", x);
				program->relative_base += x;
				program->pc += 2;
				break;
			}
			default:
				return 1;
		}
	}
	return 0;
}


// Communications

struct channel {
	struct program *program;
	int server_sockfd;
	int client_sockfd;
};

int open_server_socket(int port) {
    struct protoent *protoent = getprotobyname("tcp");
    if (!protoent) {
        perror("getprotobyname");
        exit(EXIT_FAILURE);
    }

    int server_sockfd = socket(AF_INET, SOCK_STREAM, protoent->p_proto);
    if (server_sockfd == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    int enable = 1;
    if (setsockopt(server_sockfd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(enable)) < 0) {
        perror("setsockopt(SO_REUSEADDR) failed");
        exit(EXIT_FAILURE);
    }

    struct sockaddr_in server_address;
    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    server_address.sin_port = htons(5000);
    if (bind(server_sockfd, (struct sockaddr*)&server_address, sizeof(server_address)) == -1) {
        perror("bind");
        exit(EXIT_FAILURE);
    }

    if (listen(server_sockfd, 5) == -1) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
    fprintf(stderr, "listening on port %d\n", port);
    return server_sockfd;
}

opcode channel_input(void *io_context) {
	struct channel *c = (struct channel *)io_context;
	char buf = 0;

	do {
		int r = read(c->client_sockfd, &buf, 1);
		if (r != 1) {
			return 0;
		}
	} while (buf < '1' || '4' < buf);

	opcode in = buf - '0';

	if (debug_channel) {
		printf("channel read %d\n", in);
	}

	return in;
}

void channel_output(void *io_context, opcode data) {
	struct channel *c = (struct channel *)io_context;
	char buf = (char)(data + '0');

	if (debug_channel) {
		printf("channel write %d\n", data);
	}

	if (write(c->client_sockfd, &buf, 1) != 1) {
		perror("write");
	}
}

struct channel *new_channel(void) {
	struct channel *c = (struct channel *)malloc(sizeof(struct channel));
	c->server_sockfd = open_server_socket(5000);
	return c;
}

void free_channel(struct channel *c) {
	close(c->server_sockfd);
	free(c);	
}

void run_channel(struct channel *c, struct program *program) {
	c->program = copy_program(program);
	c->program->input = channel_input;
	c->program->output = channel_output;
	c->program->io_context = c;

	struct sockaddr_in client_address;
	socklen_t client_len = sizeof(client_address);
	c->client_sockfd = accept(c->server_sockfd, (struct sockaddr *)&client_address, &client_len);
	printf("accepted connection\n");

	opcode exit = run_program(c->program, 0);
	printf("exit code %d\n", exit);

	close(c->client_sockfd);
	free_program(c->program);
}


int main(int argc, char **argv) {
	struct program *program = load_program("input.txt");
	struct channel *channel = new_channel();

	while (1) {
		run_channel(channel, program);
	}

	free_channel(channel);
	free_program(program);

	return 0;
}