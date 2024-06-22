ECL=$(shell which ecl)

PROG = clarafx
SLCK = clarafx-*.txz

.PHONY : compile
compile : $(PROG)

$(PROG) :
	$(ECL) -q -norc \
		--eval '(require "asdf")' \
		--eval '(asdf:load-system "clarafx")' \
		--eval '(asdf:make-build "clarafx" :type :program :move-here "./" :epilogue-code (quote (progn (clarafx.cli:main) (ext:quit 0))))' \
		--eval '(ext:quit 0)'

.PHONY : slackbuild
slackbuild : $(SLCK)
$(SLCK) : slackbuild.sh
	sh slackbuild.sh

.PHONY : clean
clean :
	rm -rf SBo

.PHONY : clean-all
clean-all : clean
	rm $(PROG) $(SLCK)
