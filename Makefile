.PHONY: all clean

all: *.hs
	ghc Linode.hs

clean:
	rm *.hi *.o Linode
