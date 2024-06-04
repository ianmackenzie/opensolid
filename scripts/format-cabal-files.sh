#!/bin/bash

cabal-gild --io=opensolid/opensolid.cabal
cabal-gild --io=opensolid-api/opensolid-api.cabal
cabal-gild --io=opensolid-ffi/opensolid-ffi.cabal
cabal-gild --io=opensolid-python/opensolid-python.cabal
cabal-gild --io=sandbox/sandbox.cabal
cabal-gild --io=test-server/test-server.cabal
