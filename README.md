# linode-haskell

This is a Haskell wrapper around the [Linode v4 API](https://developers.linode.com/reference/).

# Setup

Install the [Haskell platform](https://www.haskell.org/platform/). You will also need http-conduit:

```bash
$ cabal install http-conduit
```

$ Compile and run

```
$ ghc linode.hs
$ LINODE_TOKEN=my-token ./linode
```