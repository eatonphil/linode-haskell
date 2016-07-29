[![Build Status](https://travis-ci.org/eatonphil/linode-haskell.svg?branch=master)](https://travis-ci.org/eatonphil/linode-haskell)

# linode-haskell

This is a Haskell wrapper around the [Linode v4 API](https://developers.linode.com/reference/).
You will need an alpha account and a personal access token. You can get those
at [login.alpha.linode.com](https://login.alpha.linode.com).

## Setup

Install the [Haskell platform](https://www.haskell.org/platform/). You will also need http-conduit:

```bash
$ cabal update
$ cabal install http-conduit
```

## Compile and run

```
$ make
$ LINODE_TOKEN=my-token ./linode
```

## API Documentation

Although documentation for the Haskell wrapper does not yet exist,
feel free to look through the alpha API [documentation](https://developers.linode.com/reference/)
in the meantime.