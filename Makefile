TESTCASES=testcases
SRC=sources.cm
TIGERC=tigerc
RUNTIME=runtime.s
UNAME :-= $(shell uname -s)

ifeq ($(UNAME),Linux)
IMAGE=tigerc.x86-linux
endif
ifeq ($(UNAME),Darwin)
IMAGE=tigerc.x86-darwin
endif

.PHONY: *.spim
%.spim : %.tig ${IMAGE}
	./${TIGERC} $*.tig
	cat ${RUNTIME} >> $*.tmp
	cat $*.tig.s >> $*.tmp
	mv $*.tmp $*.spim
	rm $*.tig.s

main: ${SRC}
	ml-build sources.cm Main.main ${TIGERC}
