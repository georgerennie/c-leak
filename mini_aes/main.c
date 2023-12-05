#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "mini_aes.h"

// Note that the buffer is overwritten with each new call, so if the result
// is used after subsequent calls, it must be copied out of the buffer first
static const char* const bin_str(const block_t block) {
	static char str[20];
	for (uint8_t i = 0, idx = 0; i < 16; i++) {
		str[idx++] = ((block >> (15 - i)) & 0x0001) + '0';
		if (i % 4 == 3)
			str[idx++] = '_';
	}
	str[19] = '\0';
	return str;
}

int main() {
	srand(0);
	for (size_t i = 0; i < 5; i++) {
		const block_t p       = rand();
		const block_t k       = rand();
		const block_t c       = mini_aes_encrypt_block(p, k);
		const block_t p_prime = mini_aes_decrypt_block(c, k);

		printf("Example %zu\n", i);
		printf("p:  0b%s\n", bin_str(p));
		printf("p': 0b%s\n", bin_str(p_prime));
		printf("k:  0b%s\n", bin_str(k));
		printf("c:  0b%s\n", bin_str(c));
		assert(p == p_prime);
	}
}
