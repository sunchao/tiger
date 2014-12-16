TESTCASES=testcases
SRC=sources.cm
TIGERC=tigerc
RUNTIME=runtime.s
IMAGE=tigerc.x86-darwin

.PHONY: *.spim
%.spim : %.tig ${IMAGE}
	./${TIGERC} $*.tig
	cat ${RUNTIME} >> $*.tmp
	cat $*.tig.s >> $*.tmp
	mv $*.tmp $*.spim
	rm $*.tig.s

main: ${SRC}
	ml-build sources.cm Main.main ${TIGERC}
