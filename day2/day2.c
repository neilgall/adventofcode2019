#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef unsigned int opcode;

struct program {
	size_t size;
	opcode base[];
};

size_t program_size(size_t n) {
	return sizeof(size_t) + sizeof(opcode) * n;
}

struct program *load_program(char *filename) {
	size_t size = 100;
	struct program *program = (struct program *)malloc(program_size(size));
	size_t pos = 0;
	opcode cur = 0;
	FILE *f = fopen(filename, "rt");
	for (;;) {
		int c = fgetc(f);
		if (c == EOF || c == ',') {
			if (pos == size) {
				size *= 2;
				program = (struct program *)realloc(program, program_size(size));
			}
			program->base[pos++] = cur;
			cur = 0;
		} else if ('0' <= c && c <= '9') {
			cur = (cur * 10) + (c - '0');
		}
		if (c == EOF) break;
	}
	fclose(f);
	program->size = pos;
	return program;
}

struct program *copy_program(struct program *program) {
	size_t size = program_size(program->size);
	struct program *copy = (struct program *)malloc(size);
	memcpy(copy, program, size);
	return copy;
}

int read_program(struct program *program, size_t pos, opcode *x) {
	if (pos >= program->size) {
		return 1;
	}
	*x = program->base[pos];
	return 0;
}

int write_program(struct program *program, size_t pos, opcode value) {
	if (pos >= program->size) {
		return 1;
	}
	program->base[pos] = value;
	return 0;
}

void print_program(struct program *program) {
	printf("%u", program->base[0]);
	for (int i = 1; i < program->size; ++i) {
		printf(",%u", program->base[i]);
	}
	printf("\n");
}

int run_program(struct program *program, int debug) {
	size_t pc = 0;
	opcode i;
	while (read_program(program, pc, &i) == 0 && i != 99) {
		opcode x, y, z;
		if (read_program(program, pc+1, &x) != 0) return 1;
		if (read_program(program, x,    &x) != 0) return 1;
		if (read_program(program, pc+2, &y) != 0) return 1;
		if (read_program(program, y,    &y) != 0) return 1;
		if (read_program(program, pc+3, &z) != 0) return 1;
		if (debug) printf("pc=%u i=%u x=%u y=%u z=%u\n", pc, i, x, y, z);
		switch (i) {
			case 1: 
				if (write_program(program, z, x + y) != 0) return 1;
				break;
			case 2:
				if (write_program(program, z, x * y) != 0) return 1;
				break;
			default:
				return 1;
		}
		if (debug) print_program(program);
		pc += 4;
	}
	return 0;
}

void modify_program(struct program *program) {
	program->base[1] = 12;
	program->base[2] = 2;
}

void part1(struct program *original) {
	printf("Part 1\n");
	struct program *program = copy_program(original);
	modify_program(program);
	run_program(program, 0);
	print_program(program);
	free(program);	
}

void part2(struct program *original) {
	opcode noun = 0;
	opcode verb = 0;
	while (noun < 100 && verb < 100) {
		struct program *program = copy_program(original);
		write_program(program, 1, noun);
		write_program(program, 2, verb);
		if (run_program(program, 0) == 0 && program->base[0] == 19690720) {
			printf("noun = %u\nverb = %u\nresult = %u\n", noun, verb, 100*noun+verb);
			return;
		}

		if (++noun > 99) {
			noun = 0;
			++verb;
		}
	}
}

static struct program test_program = {
	.size = 12,
	.base = { 1,9,10,3,2,3,11,0,99,30,40,50 }
};

int main(int argc, char **argv) {
	struct program *program = (argc < 2) ? load_program("input.txt") : &test_program;
	part1(program);
	part2(program);

	if (program != &test_program) {
		free(program);
	}
	return 0;
}