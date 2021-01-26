DST  := ../lecture-MPRI-2.4-nbe-public
ROOT := dune-project .gitignore .ocamlformat runtest.sh CONTRIBUTING.md
SRC  := $(shell git ls-files src/)

.PHONY: export
export:
# Make sure everything compiles.
	@ dune runtest
# Recreate the destination directory from scratch.
	@ rm -rf $(DST)
	@ mkdir $(DST) $(DST)/src
# Copy the files that must reside at the root.
	@ cp $(ROOT) $(DST)
	@ cat README.md data/README.md > $(DST)/README.md
# Copy the source files.
	@ for f in $(SRC) ; do \
	  sed -f sanitize.sed $$f > $(DST)/$$f ; \
	done
# Compile the code that is given to the students.
	@ cd $(DST); dune build; dune build @fmt --auto-promote; true
# Show what we have done.
	ls $(DST) $(DST)/src
