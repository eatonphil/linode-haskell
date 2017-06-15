[![Build Status](https://travis-ci.org/eatonphil/linode-haskell.svg?branch=master)](https://travis-ci.org/eatonphil/linode-haskell)

# linode-haskell

This is a Haskell wrapper around the [Linode v4 API](https://developers.linode.com/reference/).
You will need a personal access token. You can get those by logging in with your Linode Manager credentials
at [cloud.linode.com](https://cloud.linode.com).

## Setup

Install the [Haskell platform](https://www.haskell.org/platform/).

### From cabal

```bash
$ cabal update
$ cabal install linode-v4
```

### From git

```bash
$ cabal update
$ git clone git@github.com:eatonphil/linode-haskell
$ cabal install
```

## Test

```bash
$ cabal install --enable-tests --only-dependencies
$ cabal test
```

## Example

```
$ make example
$ LINODE_TOKEN=my-token ./example/linode
```

## API Documentation

Although documentation for the Haskell wrapper does not yet exist,
feel free to look through the API v4 [documentation](https://developers.linode.com/reference/)
in the meantime.
