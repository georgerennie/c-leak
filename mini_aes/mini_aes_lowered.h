#pragma once
#include <stdint.h>

uint16_t lowered_mini_aes_encrypt_block(uint16_t, uint16_t);
uint16_t lowered_mini_aes_decrypt_block(uint16_t, uint16_t);

void lowered_mini_aes_check_enc_dec(uint16_t, uint16_t);
void lowered_mini_aes_check_dec_enc(uint16_t, uint16_t);
