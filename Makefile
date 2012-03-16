default:
	@echo "You must specify a problem to run"
	@exit 1

RTS = +RTS -M512m

%: %.hs
	ghc --make -O2 -rtsopts $<
	@echo ./$@
	@trap 'rm $@ $@.hi $@.o' EXIT && (if [[ -z "$$TIMED" ]]; then ./$@ $(RTS); else time ./$@ $(RTS); fi)

clean:
	rm -rf *.o *.hi *~
	rm -rf Euler/*{.o,.hi,~}

.PHONY: clean
