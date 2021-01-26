#!/bin/sh

FILENAME=$1

dune runtest &>/dev/null

echo "Selectively run tests for $FILENAME"
./_build/default/src/.nbe.inline-tests/inline_test_runner_nbe.exe \
    inline-test-runner                                            \
    nbe                                                           \
    -source-tree-root ..                                          \
    -only-test $1
if [ $? -eq 0 ]; then
    echo "OK"
fi
