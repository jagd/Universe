.PHONY: all clean doc

sources:=Main.hs Widget.hs Space.hs

# GHCFLAGS:=--make -O2 -prof -auto-all -caf-all
# GHCFLAGS:=--make -O1 -funbox-strict-fields -fvia-C -optc-O2

GHCFLAGS:=--make -O2

all: space

space: $(sources)
	ghc  $(GHCFLAGS) $^ -o $@

doc: $(sources)
	haddock Brainfuck.hs -o doc -h

clean:
	-rm *.hi *.o space

distclean: clean
	-rm -r doc *.prof *.hp
