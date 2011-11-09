.PHONY: clean distclean

SOURCES = $(wildcard *.hs)

prob%: prob%.hs
	ghc --make -O2 $<

clean:
	rm -f *.o *.hi

distclean: clean
	rm -f ${SOURCES:.hs=}
