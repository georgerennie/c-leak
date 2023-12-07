# Build binary
.PHONY: mini_aes
# Run all verification targets
.PHONY: verify_all
# Verify with ESBMC
.PHONY: verify_mini_aes lower_mini_aes
# Verify for functional or leakage correctness using rosette
.PHONY: verify_lowered_mini_aes verify_leakage_mini_aes
# Helpers
.PHONY: fmt clean

TOOL := racket c-leak.rkt
RKT_SRCS := c-leak.rkt $(wildcard src/*.rkt)
TOOL_SRCS := ${RKT_SRCS} $(wildcard src/*.brag)

MINI_AES_CORE := mini_aes/mini_aes.c
MINI_AES_LOWERED := mini_aes/mini_aes_lowered.c
MINI_AES_VERIFY := mini_aes/verify_mini_aes.c
MINI_AES_SRCS := mini_aes/main.c mini_aes/mini_aes.c
MINI_AES_EXE := mini_aes/mini_aes

mini_aes: ${MINI_AES_EXE}
lower_mini_aes: ${MINI_AES_LOWERED}

verify_all: verify_mini_aes verify_lowered_mini_aes verify_leakage_mini_aes

verify_mini_aes: ${MINI_AES_VERIFY} ${MINI_AES_CORE} ${MINI_AES_LOWERED}
	esbmc \
		--k-induction \
		--overflow-check --unsigned-overflow-check \
		--ub-shift-check \
		$^

verify_lowered_mini_aes: ${MINI_AES_CORE} ${TOOL_SRCS}
	${TOOL} \
		--verify mini_aes_check_enc_dec \
		--verify mini_aes_check_dec_enc \
		--fuel 10000 \
		$<

verify_leakage_mini_aes: ${MINI_AES_CORE} ${TOOL_SRCS}
	${TOOL} \
		--leak-verify mini_aes_encrypt_block key \
		--leak-verify mini_aes_decrypt_block key \
		--fuel 5000 \
		$<

${MINI_AES_LOWERED}: ${MINI_AES_CORE} ${TOOL_SRCS}
	${TOOL} --lowered-path $@ --lowered-prefix "lowered_" $<

${MINI_AES_EXE}: ${MINI_AES_SRCS}
	${CC} -O3 -Wall -o $@ $^

fmt:
	raco fmt -i --width 100 ${RKT_SRCS}
	clang-format -i ${MINI_AES_SRCS} $(wildcard mini_aes/*.h)

clean:
	rm -f ${MINI_AES_LOWERED}
	rm -f ${MINI_AES_EXE}
