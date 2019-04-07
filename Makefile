.POSIX:
EMACS = emacs

compile: bitpack.elc bitpack-tests.elc

bitpack.elc: bitpack.el
bitpack-tests.elc: bitpack-tests.el bitpack.elc

clean:
	rm -f bitpack.elc bitpack-tests.elc

check: bitpack-tests.elc
	$(EMACS) -batch -Q -L . -l bitpack-tests.elc -f ert-run-tests-batch

bench: bitpack-tests.elc
	$(EMACS) -batch -Q -L . -l bitpack-tests.elc -f bitpack-benchmark

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<


