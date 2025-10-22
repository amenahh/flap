.PHONY: all clean

MAIN=fun

all:
	dune build $(MAIN).exe
	ln -sf _build/default/$(MAIN).exe $(MAIN)

clean:
	dune clean
	rm -fr *~ l1
