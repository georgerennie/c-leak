#include "mini_aes.c"
#include "mini_aes_lowered.c"
#include <assert.h>

uint64_t oracle();

int main() {
	const block_t key = oracle();
	const block_t plain = oracle();
	// Check that encrypting then decrypting, or decrypting then encrypting,
	// gives back the same value
	assert(mini_aes_decrypt_block(mini_aes_encrypt_block(plain, key), key) == plain);
	assert(mini_aes_encrypt_block(mini_aes_decrypt_block(plain, key), key) == plain);

	// Check that the original and lowered functions are equivalent
	assert(c_leak_mini_aes_encrypt_block(plain, key) == mini_aes_encrypt_block(plain, key));
	assert(c_leak_mini_aes_decrypt_block(plain, key) == mini_aes_decrypt_block(plain, key));
}
