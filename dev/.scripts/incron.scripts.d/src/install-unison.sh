#!/bin/bash

# get dependencies
yum -y install jq incron ocaml ocaml-camlp4-devel ctags ctags-etags

mkdir -p ~/Downloads
cd ~/Downloads
rm -rf unison-*
# get current stable
wget    --recursive                             \
        --timestamping                          \
        --no-directories                        \
        --level 1                               \
        --cut-dirs 1                            \
        --accept tar.gz				\
        --directory-prefix "$(pwd)"		\
	http://www.seas.upenn.edu/~bcpierce/unison/download/releases/stable

# unpack
tar xvfz unison-*.tar.gz
cd unison-*
make

# install binaries
cp -vf unison           /usr/local/sbin/.
cp -vf unison-fsmonitor /usr/local/sbin/.

