#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mini_aes.h"

// Note that the buffer is overwritten with each new call, so if the result
// is used after subsequent calls, it must be copied out of the buffer first
static const char* bin_str(const block_t block) {
	static char str[20];
	for (uint8_t i = 0, idx = 0; i < 16; i++) {
		str[idx++] = ((block >> (15 - i)) & 0x0001) + '0';
		if (i % 4 == 3)
			str[idx++] = '_';
	}
	str[19] = '\0';
	return str;
}

int main(int argc, char* argv[]) {
	srand(0);

	if (argc == 4) {
		const bool encrypt = !strcmp(argv[1], "e");
		const bool decrypt = !strcmp(argv[1], "d");
		if (!encrypt && !decrypt) {
			fprintf(stderr, "First argument should be \"e\" or \"d\". Got \"%s\"\n", argv[1]);
			return EXIT_FAILURE;
		}

		const block_t source = strtol(argv[2], NULL, 0);
		const block_t key    = strtol(argv[3], NULL, 0);

		if (encrypt) {
			printf("plain:  %04X\n", source);
			printf("key:    %04X\n", key);
			printf("cipher: %04X\n", mini_aes_encrypt_block(source, key));
		}

		if (decrypt) {
			printf("cipher: %04X\n", source);
			printf("key:    %04X\n", key);
			printf("plain:  %04X\n", mini_aes_decrypt_block(source, key));
		}

		return EXIT_SUCCESS;
	}

	puts("mini_aes <e|d> <plain> <key>\n");
	puts("Specify <e>ncode or <d>ecode, a source block and key block\n");

	if (argc != 1) {
		fprintf(stderr, "Expected 0 or 3 arguments. Got %d\n", argc - 1);
		return EXIT_FAILURE;
	}

	puts("No arguments given so verifying random inputs\n");
	const uint32_t num = 1000000;
	for (uint32_t i = 0; i < num; i++) {
		const block_t p       = rand();
		const block_t k       = rand();
		const block_t c       = mini_aes_encrypt_block(p, k);
		const block_t p_prime = mini_aes_decrypt_block(c, k);

		if (p != p_prime) {
			fprintf(stderr, "Failed verification for an input pattern:\n");
			fprintf(stderr, "plain:  0b%s\n", bin_str(p));
			fprintf(stderr, "key:    0b%s\n", bin_str(k));
			fprintf(stderr, "cipher: 0b%s\n", bin_str(c));
			fprintf(stderr, "plain': 0b%s\n", bin_str(p_prime));
			return EXIT_FAILURE;
		}

		if ((i + 1) % 25000 == 0) {
			printf("Verified %u patterns\n", i + 1);
		}
	}
}
