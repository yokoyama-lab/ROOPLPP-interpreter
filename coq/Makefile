ROCQ ?= rocq

.PHONY: all check clean

all: roopl.vo

roopl.vo: roopl.v
	$(ROCQ) c -Q . ROOPL roopl.v

# 独立した検査器で .vo を再検査する（証明の受理を二重確認する）
check: roopl.vo
	$(ROCQ) check -Q . ROOPL ROOPL.roopl

clean:
	rm -f *.vo *.vos *.vok *.glob .*.aux .lia.cache
