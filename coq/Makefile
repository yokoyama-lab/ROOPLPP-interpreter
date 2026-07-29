# Rocq 9.x があれば rocq を、無ければ Coq 8.x の coqc/coqchk を使う。
# roopl.v は `Require Import` しか使わないのでどちらでも通る。
ROCQ := $(shell command -v rocq 2>/dev/null)

ifeq ($(ROCQ),)
COMPILE = coqc -Q . ROOPL roopl.v
CHECK   = coqchk -Q . ROOPL ROOPL.roopl
else
COMPILE = $(ROCQ) c -Q . ROOPL roopl.v
CHECK   = $(ROCQ) check -Q . ROOPL ROOPL.roopl
endif

.PHONY: all check clean

all: roopl.vo

roopl.vo: roopl.v
	$(COMPILE)

# 独立した検査器で .vo を再検査する（証明の受理を二重確認する）
check: roopl.vo
	$(CHECK)

clean:
	rm -f *.vo *.vos *.vok *.glob .*.aux .lia.cache
