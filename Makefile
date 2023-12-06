.PHONY: mini_aes verify_mini_aes verify_lowered_mini_aes lower_mini_aes fmt clean

EXE := racket c-leak.rkt
RKT_SRCS := c-leak.rkt $(wildcard src/*.rkt)
TOOL_SRCS := ${RKT_SRCS} $(wildcard src/*.brag)

MINI_AES_CORE := mini_aes/mini_aes.c
MINI_AES_LOWERED := mini_aes/mini_aes_lowered.c
MINI_AES_VERIFY := mini_aes/verify_mini_aes.c
MINI_AES_SRCS := mini_aes/main.c mini_aes/mini_aes.c
MINI_AES_EXE := mini_aes/mini_aes

mini_aes: ${MINI_AES_EXE}
lower_mini_aes: ${MINI_AES_LOWERED}

verify_mini_aes: ${MINI_AES_VERIFY} ${MINI_AES_CORE} ${MINI_AES_LOWERED}
	esbmc \
		--k-induction --parallel-solving \
		--overflow-check --unsigned-overflow-check \
		--ub-shift-check \
		$^

verify_lowered_mini_aes: ${MINI_AES_CORE} ${TOOL_SRCS}
	${EXE} \
		--verify mini_aes_check_enc_dec \
		--verify mini_aes_check_dec_enc \
		--fuel 5000 \
		$<

${MINI_AES_LOWERED}: ${MINI_AES_CORE} ${TOOL_SRCS}
	${EXE} --lowered-path $@ --lowered-prefix "lowered_" $<

${MINI_AES_EXE}: ${MINI_AES_SRCS}
	${CC} -O3 -Wall -o $@ $^

fmt:
	raco fmt -i --width 100 ${RKT_SRCS}

clean:
	rm -f ${MINI_AES_LOWERED}
	rm -f ${MINI_AES_EXE}
