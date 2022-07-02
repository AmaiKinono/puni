# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

SHELL = /bin/sh

.PHONY: check
check: test compile style

.PHONY: test
test:
	$(SHELL) scripts/test.sh

.PHONY: compile
compile: clean
	$(SHELL) scripts/compile.sh

.PHONY: style
style:
	$(SHELL) scripts/style.sh

.PHONY: toc
toc:
	$(SHELL) scripts/toc.sh

.PHONY: clean
clean:
	rm -f ./*.elc
