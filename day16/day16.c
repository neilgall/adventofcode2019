#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef int32_t elem;

char input[1000];

size_t load_input(void) {
	FILE *f = fopen("input.txt", "rt");
	fgets(input, sizeof(input), f);
	fclose(f);
	size_t sz = strlen(input);
	while (!isdigit(input[sz-1]))
		sz--;
	input[sz] = '\0';
	return sz;
}

void parse(char *in, elem *out, size_t len) {
	for (size_t i = 0; i < len; ++i) 
		*out++ = *in++ - '0';
}

void show(elem *in, char *out, size_t len) {
	for (size_t i = 0; i < len; ++i) {
		*out++ = *in++ + '0';
	}
	*out = '\0';
}

int pattern_at(int row, int col) {
	static int pattern[4] = { 0, 1, 0, -1 };
	int i = (col / row) % 4;
	return pattern[i];
}

int output_row(elem *in, size_t len, int row) {
	int t = 0;
	for (int col = 0; col < len; ++col) {
		t += in[col] * pattern_at(row+1, col+1);
	}
	return abs(t) % 10;
}

void fft_phase(elem *in, elem *out, size_t len) {
	for (int row = 0; row < len; ++row) {
		out[row] = output_row(in, len, row);
		if (row % 1000 == 0) {
			printf("\rrow %d", row);
			fflush(stdout);
		}
	}
}

void swap(elem **a, elem **b) {
	elem *x = *a;
	*a = *b;
	*b = x;
}

void run_phases(elem *in, elem *out, size_t len, int phases) {
	size_t sz = sizeof(elem) * len;
	elem *a = (elem *)malloc(sz);
	elem *b = (elem *)malloc(sz);
	memcpy(a, in, sz);

	for (int i = 0; i < phases; ++i) {
		fft_phase(a, b, len);
		swap(&a, &b);

		printf("\r%d..", i);
		fflush(stdout);
	}

	memcpy(out, a, sz);
	free(a);
	free(b);
}

void run_phases_s(char *in_s, char *out_s, size_t out_max, int phases) {
	int len = strlen(in_s);
	elem *in = (elem *)malloc(sizeof(elem) * len);
	parse(in_s, in, len);

	elem *out = (elem *)malloc(sizeof(elem) * len);
	run_phases(in, out, len, phases);

	show(out, out_s, out_max);
	free(in);
	free(out);
}

void message_extract(char *in_s, char *out_s, int out_max, int repeat, int phases) {
	int len = strlen(in_s);
	int sz = len * sizeof(elem);
	elem *in = (elem *)malloc(sz * repeat);

	parse(in_s, in, len);
	for (int i = 1; i < repeat; ++i) {
		memcpy((void *)in + sz * i, in, sz);
	}

	elem *out = (elem *)malloc(sz * repeat);
	run_phases(in, out, len * repeat, phases);

	int offset = 0;
	for (int i = 0; i < 7; ++i) {
		offset = (offset * 10) + in[i];
	}

	show(out + offset, out_s, out_max);

	free(in);
	free(out);
}

void assert_fft_phases(char *in_s, char *expect_s, int phases) {
	char out_s[9];
	run_phases_s(in_s, out_s, 8, phases);
	assert(strncmp(out_s, expect_s, strlen(expect_s)) == 0);
}

void test_fft_phases(void) {
	assert_fft_phases("12345678", "48226158", 1);
	assert_fft_phases("12345678", "34040438", 2);
	assert_fft_phases("12345678", "03415518", 3);
	assert_fft_phases("12345678", "01029498", 4);

	assert_fft_phases("80871224585914546619083218645595", "24176176", 100);
	assert_fft_phases("19617804207202209144916044189917", "73745418", 100);
	assert_fft_phases("69317163492948606335995924319873", "52432133", 100);
}

void assert_message_extract(char *in_s, char *expect_s) {
	char out[9];
	message_extract(in_s, out, 8, 10000, 100);
	assert(strncmp(out, expect_s, strlen(expect_s)) == 0);
}

void test_message_extract(void) {
	assert_message_extract("03036732577212944063491565474664", "84462026");
}

void part1(char *in_s) {
	char out_s[9];
	run_phases_s(in_s ,out_s, 8, 100);
	printf("Part 1 .. %s\n", out_s);
}

void part2(char *in_s) {
	char out_s[9];
	message_extract(in_s, out_s, 8, 10000, 100);
	printf("Part 2 .. %s\n", out_s);
}

int main() {
	size_t len = load_input();

	test_fft_phases();
	part1(input);

	test_message_extract();
	part2(input);

	return 0;
}