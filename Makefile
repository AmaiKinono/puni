# Copyright (C) 2021 Hao WANG
# License: GPL v3, or (at your option) any later version

SHELL = /bin/sh

.PHONY: check
check: compile style

.PHONY: compile
compile: clean
	$(SHELL) scripts/compile.sh

.PHONY: style
style:
	$(SHELL) scripts/style.sh

.PHONY: clean
clean:
	rm -f ./*.elc
