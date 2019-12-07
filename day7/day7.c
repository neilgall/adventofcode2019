#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef int opcode;

struct buffer {
	size_t size;
	size_t write_pos;
	size_t read_pos;
	opcode base[];
};

struct program {
	size_t size;
	struct buffer *input;
	struct buffer *output;
	opcode base[];
};

struct amplifier_circuit {
	size_t count;
	struct program *program[];
};

enum parameter_mode {
	position_mode = 0,
	immediate_mode = 1
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
	if (buffer->read_pos >= buffer->write_pos) {
		return 1;
	}
	*value = buffer->base[buffer->read_pos++];
	return 0;
}

void print_buffer(struct buffer *buffer) {
	for (size_t i = 0; i < buffer->write_pos; ++i) {
		if (i == buffer->read_pos)
			printf("<%d> ", buffer->base[i]);
		else
			printf("%d ", buffer->base[i]);
	}
	printf("\n");
}

size_t program_size(size_t n) {
	return sizeof(struct program) + sizeof(opcode) * n;
}

struct program *load_program(char *filename) {
	size_t size = 100;
	struct program *program = (struct program *)malloc(program_size(size));
	size_t pos = 0;
	opcode cur = 0;
	opcode sign = 1;
	FILE *f = fopen(filename, "rt");
	for (;;) {
		int c = fgetc(f);
		if (c == EOF || c == ',') {
			if (pos == size) {
				size *= 2;
				program = (struct program *)realloc(program, program_size(size));
			}
			program->base[pos++] = cur * sign;
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
	program->size = pos;
	program->input = new_buffer(100);
	program->output = new_buffer(100);
	return program;
}

void free_program(struct program *program) {
	free(program->input);
	free(program->output);
	free(program);
}

struct program *copy_program(struct program *program) {
	size_t size = program_size(program->size);
	struct program *copy = (struct program *)malloc(size);
	memcpy(copy, program, size);
	copy->input = new_buffer(100);
	copy->output = new_buffer(100);
	return copy;
}

enum parameter_mode parameter_mode_from_opcode(opcode x, size_t index) {
	switch (x) {
		case 3:
			return immediate_mode;
		case 4:
			return position_mode;
		default:
			if (index == 3) 
				return immediate_mode;
			else {
				assert(index >= 1);
				x /= 100;
				while (--index) x /= 10;
				return (x % 10 == 0) ? position_mode : immediate_mode;
			}
	}
}

int read_opcode(struct program *program, size_t pc, opcode *x) {
	if (pc >= program->size) {
		return 1;
	}
	*x = program->base[pc];
	return 0;
}

int read_parameter(struct program *program, size_t pc, size_t index, opcode *x) {
	if (pc + index >= program->size) {
		return 1;
	}
	enum parameter_mode mode = parameter_mode_from_opcode(program->base[pc], index);
	opcode immediate = program->base[pc + index];
	switch (mode) {
		case position_mode:
			if (immediate >= program->size) {
				return 1;
			}
			*x = program->base[immediate];
			break;

		case immediate_mode:
			*x = immediate;
			break;
	}

	return 0;
}

int write_program(struct program *program, size_t pos, opcode value) {
	if (pos >= program->size) {
		return 1;
	}
	program->base[pos] = value;
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
	printf("%u", program->base[0]);
	for (int i = 1; i < program->size; ++i) {
		printf(",%d", program->base[i]);
	}
	printf("\n");
}

int run_program(struct program *program, int debug) {
	size_t pc = 0;
	opcode i;
	while (read_opcode(program, pc, &i) == 0 && i != 99) {
		switch (i % 100) {
			case 1: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 1001;
				if (read_parameter(program, pc, 2, &y) != 0) return 1002;
				if (read_parameter(program, pc, 3, &z) != 0) return 1003;
				if (debug) printf("pc=%u i=%d x=%d y=%d z=%d\n", pc, i, x, y, z);
				if (write_program(program, z, x + y) != 0) return 1004;
				pc += 4;
				break;
			}
			case 2: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 2001;
				if (read_parameter(program, pc, 2, &y) != 0) return 2002;
				if (read_parameter(program, pc, 3, &z) != 0) return 2003;
				if (debug) printf("pc=%u i=%d x=%d y=%d z=%d\n", pc, i, x, y, z);
				if (write_program(program, z, x * y) != 0) return 2004;
				pc += 4;
				break;
			}
			case 3: {
				opcode x, z;
				if (read_buffer(program->input, &x) != 0) return 3001;
				if (read_parameter(program, pc, 1, &z) != 0) return 3002;
				if (debug) printf("pc=%u i=%d x=%d z=%d\n", pc, i, x, z);
				if (write_program(program, z, x) != 0) return 3004;
				pc += 2;
				break;
			}
			case 4: {
				opcode x;
				if (read_parameter(program, pc, 1, &x) != 0) return 4001;
				if (debug) printf("pc=%u i=%d x=%d\n", pc, i, x);
				if (write_output(program, x) != 0) return 4004;
				pc += 2;
				break;
			}
			case 5: {
				opcode x, y;
				if (read_parameter(program, pc, 1, &x) != 0) return 5001;
				if (read_parameter(program, pc, 2, &y) != 0) return 5002;
				pc = (x != 0) ? y : pc + 3;
				break;
			}
			case 6: {
				opcode x, y;
				if (read_parameter(program, pc, 1, &x) != 0) return 6001;
				if (read_parameter(program, pc, 2, &y) != 0) return 6002;
				pc = (x == 0) ? y : pc + 3;
				break;
			}
			case 7: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 7001;
				if (read_parameter(program, pc, 2, &y) != 0) return 7002;
				if (read_parameter(program, pc, 3, &z) != 0) return 7003;
				if (write_program(program, z, (x < y) ? 1 : 0) != 0) return 7004;
				pc += 4;
				break;
			}
			case 8: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 8001;
				if (read_parameter(program, pc, 2, &y) != 0) return 8002;
				if (read_parameter(program, pc, 3, &z) != 0) return 8003;
				if (write_program(program, z, (x == y) ? 1 : 0) != 0) return 8004;
				pc += 4;
				break;
			}
			default:
				return 1;
		}
	}
	return 0;
}

struct amplifier_circuit *new_amplifier_circuit(struct program *program, size_t count) {
	struct amplifier_circuit *ac = (struct amplifier_circuit *)malloc(sizeof(struct amplifier_circuit) + sizeof(struct program *) * count);
	ac->count = count;
	for (size_t i = 0; i < count; ++i) 
		ac->program[i] = copy_program(program);
	return ac;	
}

void free_amplifier_circuit(struct amplifier_circuit *ac) {
	for (size_t i = 0; i < ac->count; ++i) {
		free_program(ac->program[i]);
	}
	free(ac);
}

void print_phases(opcode phases[], size_t count) {
	for (size_t i = 0; i < count; ++i) {
		printf("%d ", phases[i]);
	}
	printf("\n");
}

void rotate(opcode *data, size_t count) {
	opcode first = data[0];
	for (size_t i = 0; i < count-1; ++i)
		data[i] = data[i+1];
	data[count-1] = first;
}

void fill_permutations(struct buffer **buffer, size_t count, size_t fixed_count, opcode *data) {	
	for (size_t r = fixed_count; r < count; ++r) {
		rotate(data+fixed_count, count-fixed_count);
		fill_permutations(buffer, count, fixed_count + 1, data);

		if (r < count-1) {
			for (size_t i = 0; i < count; ++i) {
				*buffer = write_buffer(*buffer, data[i]);
			}
		}
	}
}

struct buffer *phase_permutations(size_t count) {
	size_t permutations = 1;
	for (size_t i = 1; i <= count; ++i) {
		permutations *= i;
	}
	
	opcode *data = malloc(sizeof(opcode) * count);
	for (size_t i = 0; i < count; ++i)
		data[i] = i;

	struct buffer *buffer = new_buffer(count * permutations);
	fill_permutations(&buffer, count, 0, data);

	free(data);
	return buffer;
}

int run_amplifier_circuit(struct amplifier_circuit *ac, opcode phases[], opcode *result) {
	for (size_t i = 0; i < ac->count; ++i) {
		opcode input = 0;
		if (i > 0 && read_buffer(ac->program[i-1]->output, &input) != 0) return 1;
		write_input(ac->program[i], phases[i]);
		write_input(ac->program[i], input);
		if (run_program(ac->program[i], 0) != 0) return 1;
	}
	
	if (read_buffer(ac->program[ac->count-1]->output, result) != 0)
		return 1;

	return 0;
}

void part1_test(struct program *original, opcode *phases) {
	printf("Part 1 Test... ");
	struct amplifier_circuit *ac = new_amplifier_circuit(original, 5);

	opcode output;
	int result = run_amplifier_circuit(ac, phases, &output);
	printf("%d\n", output);

	free_amplifier_circuit(ac);
}

int part1(struct program *program) {
#define UNITS 5
	opcode best_phases[UNITS];
	opcode best_output = 0;

	struct buffer *phases_buffer = phase_permutations(UNITS);

	int stop = 0;
	while (!stop) {
		opcode phases[UNITS];
		for (size_t i = 0; i < UNITS && !stop; ++i) {
			if (read_buffer(phases_buffer, &phases[i]) != 0) stop = 1;
		}
		if (stop) break;

		struct amplifier_circuit *ac = new_amplifier_circuit(program, UNITS);
		opcode output;
		int result = run_amplifier_circuit(ac, phases, &output);
		free_amplifier_circuit(ac);

		if (result != 0) {
			printf("Running program failed for phases ");
			print_phases(phases, UNITS);
			return 1;
		}

		if (output > best_output) {
			memcpy(best_phases, phases, sizeof(opcode) * UNITS);
			best_output = output;
		}
	}
	stop:

	printf("Best output %d for phases: ", best_output);
	print_phases(best_phases, UNITS);

	return 0;
}

// test examples

static struct program test_program_1 = {
	.size = 17,
	.base = { 3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0 }
};

static struct program test_program_2 = {
	.size = 25,
	.base = { 
		3,23,3,24,1002,24,10,24,1002,23,-1,23,
		101,5,23,23,1,24,23,23,4,23,99,0,0
	}
};

static struct program test_program_3 = {
	.size = 34,
	.base = {
		3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
		1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
	}
};


int main(int argc, char **argv) {
	printf("Running Tests..\n");
	part1(&test_program_1);
	part1(&test_program_2);
	part1(&test_program_3);

	printf("\nPart 1\n");
	struct program *program = load_program("input.txt");
	part1(program);
	free_program(program);

	return 0;
}