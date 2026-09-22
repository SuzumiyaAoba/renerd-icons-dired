EMACS ?= emacs
LOAD_PATH_FLAGS ?=

.PHONY: check compile test benchmark clean

compile:
	$(EMACS) -Q --batch -L . $(LOAD_PATH_FLAGS) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile renerd-icons-dired.el

check: compile test

test:
	$(EMACS) -Q --batch -L . $(LOAD_PATH_FLAGS) \
	  -l test/renerd-icons-dired-test.el \
	  -f ert-run-tests-batch-and-exit

benchmark:
	$(EMACS) -Q --batch -L . $(LOAD_PATH_FLAGS) \
	  -l bench-renerd-icons-dired.el

clean:
	rm -f renerd-icons-dired.elc
