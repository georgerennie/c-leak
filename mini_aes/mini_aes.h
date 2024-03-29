#pragma once
#include <stdint.h>

typedef uint16_t block_t;

block_t mini_aes_encrypt_block(const block_t plain, const block_t key);
block_t mini_aes_decrypt_block(const block_t cipher, const block_t key);

void mini_aes_check_enc_dec(const block_t plain, const block_t key);
void mini_aes_check_dec_enc(const block_t cipher, const block_t key);
