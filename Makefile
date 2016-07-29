.PHONY: example clean

example: example/linode.hs
	ghc example/linode.hs

clean:
	rm example/*.hi example/*.o example/linode
