#!/bin/sh

mkdir -p ~/.config/common-lisp/source-registry.conf/
echo "(:tree \"$(pwd)\")" > ~/.config/common-lisp/source-registry.conf/10-cogworld.conf
