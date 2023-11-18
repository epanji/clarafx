ECL=$(shell which ecl)

PROG = clarafx

.PHONY : compile
compile : $(PROG)

$(PROG) :
	$(ECL) -q -norc \
		--eval '(require "asdf")' \
		--eval '(asdf:load-system "clarafx")' \
		--eval '(asdf:make-build "clarafx" :type :program :move-here "./" :epilogue-code (quote (progn (clarafx.cli:main) (ext:quit 0))))' \
		--eval '(ext:quit 0)'

.PHONY : clean
clean :
	rm $(PROG)
