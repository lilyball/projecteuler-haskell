default:
	@echo "You must specify a problem to run"
	@exit 1

%: %.hs
	ghc --make -O $<
	@echo ./$@
	@trap 'rm $@ $@.hi $@.o' EXIT && ./$@

clean:
	rm -rf *.o *.hi *~

.PHONY: clean
