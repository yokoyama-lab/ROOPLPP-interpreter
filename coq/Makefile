# Rocq 9.x があれば rocq を、無ければ Coq 8.x の coqc/coqchk を使う。
# roopl.v は `Require Import` しか使わないのでどちらでも通る。
ROCQ := $(shell command -v rocq 2>/dev/null)

ifeq ($(ROCQ),)
COMPILE = coqc -Q . ROOPL
CHECK   = coqchk -Q . ROOPL ROOPL.roopl ROOPL.roopl_small
else
COMPILE = $(ROCQ) c -Q . ROOPL
CHECK   = $(ROCQ) check -Q . ROOPL ROOPL.roopl ROOPL.roopl_small
endif

.PHONY: all check extract clean

all: roopl.vo roopl_small.vo

roopl.vo: roopl.v
	$(COMPILE) roopl.v

# 小ステップ意味論は大ステップ側の定義を使う
roopl_small.vo: roopl_small.v roopl.vo
	$(COMPILE) roopl_small.v

# 独立した検査器で .vo を再検査する（証明の受理を二重確認する）
check: roopl.vo
	$(CHECK)

# 検証済みインタプリタを OCaml へ抽出する（生成物は extracted/ にコミット済み）
extract: roopl.vo
ifeq ($(ROCQ),)
	coqc -Q . ROOPL extract.v
else
	$(ROCQ) c -Q . ROOPL extract.v
endif

clean:
	rm -f *.vo *.vos *.vok *.glob .*.aux .lia.cache
