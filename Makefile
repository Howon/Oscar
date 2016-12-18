TARGET=oscar.native
OSCAR=oscar
STDLIB=stdlib.oscar
NATIVE=native
ACTOR=$(NATIVE)/actor
IMMUT=$(NATIVE)/immut

CXX= clang
CXXFLAGS = -std=c++14 -Wall -g -c -O3

LIBS=-I,/usr/lib/ocaml/
OCAMLLIBS=-I/usr/lib/ocaml
OCAMLFLAGS= ocamlopt -linkpkg -package llvm -package llvm.analysis

OCAMLC=ocamlfind

BUILDDIR=include

oscar :
	@echo 'Buiding Oscar Compiler'
	@cd ./src && \
	eval `opam config env` && \
	make && \
	cd ../ && \
	mkdir -p $(BUILDDIR) && \
	mv ./src/$(TARGET) ./$(OSCAR) && \
	cp ./src/$(STDLIB) ./$(BUILDDIR) && \
	cp ./src/$(IMMUT)/* ./$(BUILDDIR) && \
	echo 'Oscar Compiler Succesfully Built'

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

.PHONY : all clean

all: clean oscar

clean :
	@echo 'Cleaning Oscar build'
	@cd ./src/ && \
	rm -f oscar parser.ml parser.mli scanner.ml *.cmo *.cmi *.cmx *.o && \
	rm -rf _build && \
	cd ../ && \
	rm -rf $(BUILDDIR) ./oscar && \
	echo Oscar build cleaned
