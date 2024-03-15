#include "mini_aes.h"
#include <assert.h>
#include <stdbool.h>

static const uint8_t s_box[16u] = {
    0b1110, // 0000
    0b0100, // 0001
    0b1101, // 0010
    0b0001, // 0011
    0b0010, // 0100
    0b1111, // 0101
    0b1011, // 0110
    0b1000, // 0111
    0b0011, // 1000
    0b1010, // 1001
    0b0110, // 1010
    0b1100, // 1011
    0b0101, // 1100
    0b1001, // 1101
    0b0000, // 1110
    0b0111  // 1111
};

static const uint8_t inv_s_box[16u] = {
    0b1110, // 0000
    0b0011, // 0001
    0b0100, // 0010
    0b1000, // 0011
    0b0001, // 0100
    0b1100, // 0101
    0b1010, // 0110
    0b1111, // 0111
    0b0111, // 1000
    0b1101, // 1001
    0b1001, // 1010
    0b0110, // 1011
    0b1011, // 1100
    0b0010, // 1101
    0b0000, // 1110
    0b0101  // 1111
};

static uint8_t secure_s_box(const uint8_t idx) {
	// Iterate through all indices to prevent cache timing side-channel from
	// s-box lookup
	uint8_t result = 0u;
	for (uint8_t i = 0u; i < 0x10; i += 1u) {
		const bool match = (idx == i);
		// Basic version: result |= s_box * match;
		// This is vulnerable to a timing side-channel for non constant time
		// multiplication (like on some basic microcontrollers) so we replace
		// it with a 4-bit mask
		const uint8_t mask = match | (match << 1u) | (match << 2u) | (match << 3u);
		result |= mask & s_box[i];
	}
	return result;
}

// Same security hardening as secure_s_box
static uint8_t secure_inv_s_box(const uint8_t idx) {
	uint8_t result = 0u;
	for (uint8_t i = 0u; i < 0x10; i += 1u) {
		const bool    match = (idx == i);
		const uint8_t mask  = match | (match << 1u) | (match << 2u) | (match << 3u);
		result |= mask & inv_s_box[i];
	}
	return result;
}

static block_t nibble_sub(const block_t block) {
	block_t new_block = 0u;
	for (uint8_t i = 0u; i < 16u; i += 4u) {
		new_block |= secure_s_box((block >> i) & 0x000F) << i;
	}

	return new_block;
}

static block_t inv_nibble_sub(const block_t block) {
	block_t new_block = 0u;
	for (uint8_t i = 0u; i < 16u; i += 4u) {
		new_block |= secure_inv_s_box((block >> i) & 0x000F) << i;
	}

	return new_block;
}

static block_t shift_row(const block_t block) {
	// (c0, c1, c2, c3) = (b0, b3, b2, b1)
	block_t new_block = block & 0xF0F0;
	new_block |= (block << 8u) & 0x0F00;
	new_block |= (block >> 8u) & 0x000F;
	return new_block;
}

// This function is constant time in a, but not in b. For our uses this is
// fine, as one operand is always constant
static uint8_t gf_mul(uint8_t a, uint8_t b) {
	uint8_t result = 0u;
	while (b > 0u) {
		// Repeatedly add a (shifted) for each bit of b
		if (b & 0x01) {
			result ^= a;
		}
		a <<= 1u;

		// Reduce the result modulo the characteristic
		// This naive version is vulnerable to timing attacks as it causes
		// secret dependent divergent control flow
		// if (a & 0b10000) { a ^= 0b10011; }
		//
		// This masked version is slightly better, but can leak information
		// through the multiplication operation on microcontrollers without
		// constant time multiplication
		// a ^= 0b10011 * ((a & 0b10000) > 0u);
		//
		// This solution does not leak information through control flow or
		// multiplication
		const bool enable = ((a & 0b10000) > 0u);
		a ^= enable | (enable << 1u) | (enable << 4u);

		b >>= 1u;
	}
	return result;
}

static block_t mix_column(const block_t block) {
	const block_t c0 = (block & 0xF000) >> 12u;
	const block_t c1 = (block & 0x0F00) >> 8u;
	const block_t c2 = (block & 0x00F0) >> 4u;
	const block_t c3 = block & 0x000F;

	block_t new_block = 0u;

	// d0 = 3*c0 + 2*c1
	// d1 = 2*c0 + 3*c1
	new_block |= (gf_mul(c0, 3u) ^ gf_mul(c1, 2u)) << 12u;
	new_block |= (gf_mul(c0, 2u) ^ gf_mul(c1, 3u)) << 8u;
	// d2 = 3*c2 + 2*c3
	// d3 = 2*c2 + 3*c3
	new_block |= (gf_mul(c2, 3u) ^ gf_mul(c3, 2u)) << 4u;
	new_block |= (gf_mul(c2, 2u) ^ gf_mul(c3, 3u));

	return new_block;
}

static block_t next_key(const block_t previous_key, const uint8_t round) {
	// w4 = w0 ^ perturb
	// w5 = w1 ^ w0 ^ perturb
	// w6 = w2 ^ w1 ^ w0 ^ perturb
	// w7 = w3 ^ w2 ^ w1 ^ w0 ^ perturb
	const uint8_t perturb_nibble = secure_s_box(previous_key & 0x0F) ^ round;
	const uint8_t perturb_byte   = perturb_nibble | (perturb_nibble << 4u);
	const block_t perturb_block  = perturb_byte | (perturb_byte << 8u);

	block_t key = previous_key ^ perturb_block;
	for (uint8_t i = 1u; i < 4u; i += 1u) {
		key ^= previous_key >> (4u * i);
	}

	return key;
}

block_t mini_aes_encrypt_block(const block_t plain, const block_t key) {
	const block_t k0 = key;
	const block_t k1 = next_key(k0, 1u);
	const block_t k2 = next_key(k1, 2u);

	const block_t c0 = plain ^ k0;
	const block_t c1 = mix_column(shift_row(nibble_sub(c0))) ^ k1;
	const block_t c2 = shift_row(nibble_sub(c1)) ^ k2;

	return c2;
}

block_t mini_aes_decrypt_block(const block_t cipher, const block_t key) {
	const block_t k0 = key;
	const block_t k1 = next_key(k0, 1u);
	const block_t k2 = next_key(k1, 2u);

	const block_t c1    = inv_nibble_sub(shift_row(cipher ^ k2));
	const block_t c0    = inv_nibble_sub(shift_row(mix_column(c1 ^ k1)));
	const block_t plain = c0 ^ k0;

	return plain;
}

// These two self checking functions are used to aid verification as they can
// be verified both with esbmc (see verify_mini_aes.c) and clique
void mini_aes_check_enc_dec(const block_t plain, const block_t key) {
	assert(mini_aes_encrypt_block(mini_aes_decrypt_block(plain, key), key) == plain);
}

void mini_aes_check_dec_enc(const block_t cipher, const block_t key) {
	assert(mini_aes_decrypt_block(mini_aes_encrypt_block(cipher, key), key) == cipher);
}
