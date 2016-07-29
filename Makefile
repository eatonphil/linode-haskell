.PHONY: all clean

all: *.hs
	ghc request.hs response.hs api.hs linode.hs

clean:
	rm *.hi *.o linode
