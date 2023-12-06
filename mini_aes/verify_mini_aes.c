#include "mini_aes.h"
#include "mini_aes_lowered.h"
#include <assert.h>

uint64_t oracle();

int main() {
	const block_t key = oracle();
	const block_t plain = oracle();
	// Check that encrypting then decrypting, or decrypting then encrypting,
	// gives back the same value
	mini_aes_check_enc_dec(key, plain);
	mini_aes_check_dec_enc(key, plain);
	lowered_mini_aes_check_enc_dec(key, plain);
	lowered_mini_aes_check_dec_enc(key, plain);

	// Check that the original and lowered functions are equivalent
	assert(lowered_mini_aes_encrypt_block(plain, key) == mini_aes_encrypt_block(plain, key));
	assert(lowered_mini_aes_decrypt_block(plain, key) == mini_aes_decrypt_block(plain, key));
}
