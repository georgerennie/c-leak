#include "mini_aes.h"

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

static block_t nibble_sub(const block_t block) {
	block_t new_block = 0u;
	for (uint8_t i = 0u; i < 16u; i += 4u) {
		new_block |= s_box[(block >> i) & 0x000F] << i;
	}

	return new_block;
}

static block_t inv_nibble_sub(const block_t block) {
	block_t new_block = 0u;
	for (uint8_t i = 0u; i < 16u; i += 4u) {
		new_block |= inv_s_box[(block >> i) & 0x000F] << i;
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

static uint8_t gf_mul(uint8_t a, uint8_t b) {
	uint8_t result = 0u;
	while (b > 0u) {
		if (b & 0x01) {
			result ^= a;
		}
		a <<= 1u;
		if (a & 0b10000) {
			a ^= 0b10011;
		}
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
	new_block |= (gf_mul(3u, c0) ^ gf_mul(2u, c1)) << 12u;
	new_block |= (gf_mul(2u, c0) ^ gf_mul(3u, c1)) << 8u;
	// d2 = 3*c2 + 2*c3
	// d3 = 2*c2 + 3*c3
	new_block |= (gf_mul(3u, c2) ^ gf_mul(2u, c3)) << 4u;
	new_block |= (gf_mul(2u, c2) ^ gf_mul(3u, c3));

	return new_block;
}

static block_t next_key(const block_t previous_key, const uint8_t round) {
	// w4 = w0 ^ perturb
	// w5 = w1 ^ w0 ^ perturb
	// w6 = w2 ^ w1 ^ w0 ^ perturb
	// w7 = w3 ^ w2 ^ w1 ^ w0 ^ perturb
	const block_t perturb = (s_box[previous_key & 0x0F] ^ round) * 0x1111;

	block_t key = previous_key ^ perturb;
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
