#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static int debug = 0;

typedef __int128 opcode;

struct buffer {
	size_t size;
	size_t write_pos;
	size_t read_pos;
	opcode base[];
};

struct program {
	size_t memory_size;
	size_t pc;
	size_t relative_base;
	struct buffer *input;
	struct buffer *output;
	opcode *memory;
};

enum parameter_mode {
	position_mode = 0,
	immediate_mode = 1,
	relative_mode = 2
};

size_t buffer_size(size_t size) {
	return sizeof(struct buffer) + sizeof(opcode) * size;
}

struct buffer *new_buffer(size_t size) {
	struct buffer *b = (struct buffer *)malloc(buffer_size(size));
	b->size = size;
	b->write_pos = 0;
	b->read_pos = 0;
	return b;
}

struct buffer *write_buffer(struct buffer *buffer, opcode value) {
	if (buffer->write_pos == buffer->size) {
		buffer->size *= 2;
		buffer = (struct buffer *)realloc(buffer, buffer_size(buffer->size));
	}
	buffer->base[buffer->write_pos++] = value;
	return buffer;
}

int read_buffer(struct buffer *buffer, opcode *value) {
	if (buffer->read_pos == buffer->write_pos) 
		return 1;

	*value = buffer->base[buffer->read_pos++];

	return 0;
}

char *print_opcode(opcode n) {
	static char str[50];
	if (n == 0)
		return "0";
	else {
		char *s = str + sizeof(str)-1;
		int negative = 0;
		opcode x = n;
		if (x < 0) {
			negative = 1;
			x = -x; // eek, minimum negative value edge case
		}
		for (*s-- = '0'; x != 0 && s != str; x /= 10) {
			*--s = '0' + (x % 10);
		}
		if (negative) 
			*--s = '-';
		return s; // eek, not thread safe
	}
}

void print_buffer(struct buffer *buffer) {

	for (size_t i = 0; i < buffer->write_pos; ++i) {
		if (i == buffer->read_pos)
			printf("<%s> ", print_opcode(buffer->base[i]));
		else
			printf("%s ", print_opcode(buffer->base[i]));
	}
	printf("\n");
}

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
	program->input = new_buffer(100);
	program->output = new_buffer(100);
	program->pc = 0;
	program->relative_base = 0;
	return program;
}

void free_program(struct program *program) {
	free(program->memory);
	free(program->input);
	free(program->output);
	free(program);
}

struct program *copy_program(struct program *program) {
	struct program *copy = (struct program *)malloc(sizeof(struct program));
	copy->memory_size = program->memory_size;
	copy->memory = (opcode *)malloc(memory_size(program->memory_size));
	memcpy(copy->memory, program->memory, memory_size(program->memory_size));
	copy->input = new_buffer(100);
	copy->output = new_buffer(100);
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
			return immediate_mode;
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
				printf("read index %d position mode (immediate %s) ", index, print_opcode(immediate));
				printf("--> %s\n", print_opcode(program->memory[immediate]));
			}
			*x = program->memory[immediate];
			break;
		}
		case immediate_mode: {
			if (debug > 10) printf("read index %d immediate --> %s\n", index, print_opcode(immediate));
			*x = immediate;
			break;
		}
		case relative_mode: {
			size_t address = program->relative_base + immediate;
			ensure_program_memory(program, address);
			if (debug > 10) {
				printf("read index %d relative (immediate %s ", index, print_opcode(immediate));
				printf("relative %u) --> %s\n", address, print_opcode(program->memory[address]));
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
				printf("write index %d position mode (immediate %s) ", index, print_opcode(immediate));
				printf(print_opcode(program->memory[immediate]));
				printf(" <-- %s\n", print_opcode(value));
			}
			program->memory[immediate] = value;
			break;
		}
		case relative_mode: {
			size_t address = program->relative_base + immediate;
			ensure_program_memory(program, address);
			if (debug > 10) {
				printf("write index %d relative mode (immediate %s ", index, print_opcode(immediate));
				printf("relative %u) <-- %s\n", address, print_opcode(value));
			}
			program->memory[address] = value;
			break;
		}
	}

	return 0;
}

int write_input(struct program *program, opcode value) {
	program->input = write_buffer(program->input, value);
	return 0;
}

int write_output(struct program *program, opcode value) {
	program->output = write_buffer(program->output, value);
	return 0;
}

void print_program(struct program *program) {
	printf("%s", print_opcode(program->memory[0]));
	for (int i = 1; i < program->memory_size / sizeof(opcode); ++i) {
		printf(",%s", print_opcode(program->memory[i]));
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
		printf("%04u %16s %4s ", program->pc, print_opcode(i), instr);
		printf("%s\n", print_opcode(x));	
	}
}

void debug2(struct program *program, opcode i, char *instr, opcode x, opcode y) {
	if (debug) {
		printf("%04u %16s %4s ", program->pc, print_opcode(i), instr);
		printf(print_opcode(x));
		printf(",%s\n", print_opcode(y));
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
				opcode x;
				if (read_buffer(program->input, &x) != 0) return 3001;
				debug1(program, i, "in", x);
				if (write_program(program, 1, x) != 0) return 3004;
				program->pc += 2;
				break;
			}
			case 4: {
				opcode x;
				if (read_parameter(program, 1, &x) != 0) return 4001;
				debug1(program, i, "out", x);
				if (write_output(program, x) != 0) return 4004;
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


// tests

struct program *make_test_program(opcode *source, size_t source_size) {
	struct program program = {
		.memory_size = source_size,
		.memory = (opcode *)malloc(source_size)
	};
	memcpy(program.memory, source, source_size);
	struct program *copy = copy_program(&program);
	free(program.memory);
	return copy;
}

void test_replicate_self() {
	printf("Test self-replication...");

	opcode source[] = {
		109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99
	};
	struct program *program = make_test_program(source, sizeof(source));
	int result = run_program(program, 0);

	int check = memcmp(program->output->base, source, sizeof(source)) == 0;
	printf("exit:%d correct output:%d\n", result, check);

	free_program(program);
}

void test_output_16_digit_number() {
	printf("Test output 16-digit number...");

	opcode source[] = {
		1102,34915192,34915192,7,4,7,99,0
	};
	struct program *program = make_test_program(source, sizeof(source));
	int result = run_program(program, 0);
	printf("exit %d output:", result);
	print_buffer(program->output);

	free_program(program);
}

void test_output_large_number() {
	printf("Test output large number...");

	opcode source[] = {
		104,1125899906842624,99
	};
	struct program *program = make_test_program(source, sizeof(source));
	int result = run_program(program, 0);
	printf("exit %d output:", result);
	print_buffer(program->output);

	free_program(program);
}


// solutions

int part1(struct program *original) {
	struct program *program = copy_program(original);
	write_input(program, 1);

	int result = run_program(program, 0);
	printf("exit:%d output:", result);
	print_buffer(program->output);

	free_program(program);
}


int part2(struct program *original) {
	struct program *program = copy_program(original);
	write_input(program, 2);

	int result = run_program(program, 0);
	printf("exit:%d output:", result);
	print_buffer(program->output);

	free_program(program);
}


int main(int argc, char **argv) {
	printf("Running Tests..\n");
	test_replicate_self();
	test_output_16_digit_number();
	test_output_large_number();

	struct program *program = load_program("input.txt");
	print_program(program);

	printf("\nPart 1\n");
	part1(program);

	printf("\nPart 2\n");
	part2(program);

	free_program(program);

	return 0;
}