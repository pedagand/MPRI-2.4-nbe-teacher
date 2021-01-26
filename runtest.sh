#!/bin/sh

FILENAME=$1

dune runtest &>/dev/null

echo "Selectively run tests for $FILENAME"
./_build/default/src/.src.inline-tests/inline_test_runner_src.exe \
    inline-test-runner                                            \
    src                                                           \
    -source-tree-root ..                                          \
    -only-test $1
if [ $? -eq 0 ]; then
    echo "OK"
fi
