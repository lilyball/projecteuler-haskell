default:
	@echo "You must specify a problem to run"
	@exit 1

%: %.hs
	ghc --make -O2 $<
	@echo ./$@
	@trap 'rm $@ $@.hi $@.o' EXIT && (if [[ -z "$$TIMED" ]]; then ./$@; else time ./$@; fi)

clean:
	rm -rf *.o *.hi *~

.PHONY: clean
