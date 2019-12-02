#include <stdio.h>
#include <stdlib.h>
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
	int c;
	while ((c = fgetc(f)) != EOF) {
		if (c == ',') {
			if (pos == size) {
				size *= 2;
				program = (struct program *)realloc(program, program_size(size));
			}
			program->base[pos++] = cur;
			cur = 0;
		} else if ('0' <= c && c <= '9') {
			cur = (cur * 10) + (c - '0');
		}
	}
	fclose(f);
	program->size = pos;
	return program;
}

opcode read_program(struct program *program, size_t pos) {
	assert(pos < program->size);
	return program->base[pos];
}

void write_program(struct program *program, size_t pos, opcode value) {
	assert(pos < program->size);
	program->base[pos] = value;
}

void print_program(struct program *program) {
	printf("%u", program->base[0]);
	for (int i = 1; i < program->size; ++i) {
		printf(",%u", program->base[i]);
	}
	printf("\n");
}

void run_program(struct program *program, int debug) {
	size_t pc = 0;
	opcode i;
	while ((i = read_program(program, pc)) != 99) {
		opcode x = read_program(program, read_program(program, pc+1));
		opcode y = read_program(program, read_program(program, pc+2));
		opcode z = read_program(program, pc+3);
		if (debug) printf("pc=%u i=%u x=%u y=%u z=%u\n", pc, i, x, y, z);
		switch (i) {
			case 1: 
				write_program(program, z, x + y);
				break;
			case 2:
				write_program(program, z, x * y);
				break;
			default:
				assert(0);
		}
		if (debug) print_program(program);
		pc += 4;
	}
}

void modify_program(struct program *program) {
	program->base[1] = 12;
	program->base[2] = 2;
}

static struct program test_program = {
	.size = 12,
	.base = { 1,9,10,3,2,3,11,0,99,30,40,50 }
};

int main(int argc, char **argv) {
	struct program *program;
	if (argc < 2) {
		program = load_program("input.txt");
		modify_program(program);
	} else {
		program = &test_program;
	}

	run_program(program, 0);
	print_program(program);

	if (program != &test_program) {
		free(program);
	}

	return 0;
}