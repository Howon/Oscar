TARGET=oscar.native
OSCAR=oscar
STDLIB=stdlib.oscar
NATIVE=native
ACTOR=$(NATIVE)/actor
IMMUT=$(NATIVE)/immut

CXX= clang
CXXFLAGS = -std=c++14 -Wall -g -c -O3

BUILDDIR=include

oscar : library
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

library : clean_library
	@echo 'Building Oscar Library'
	@mkdir /usr/local/include/oscar && \
	cp src/native/immut/collection.hpp  /usr/local/include/oscar/ && \
	cp src/native/immut/immut.hpp 	   	/usr/local/include/oscar/ && \
	cp src/native/immut/list.hpp		/usr/local/include/oscar/ && \
	cp src/native/immut/map.hpp         /usr/local/include/oscar/ && \
	cp src/native/immut/set.hpp         /usr/local/include/oscar/ && \
	echo 'Oscar Library Succesfully Built'

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

.PHONY : all clean clean_library

all: clean oscar

clean : clean_library
	@echo 'Cleaning Oscar Build'
	@cd ./src/ && \
	rm -f oscar parser.ml parser.mli scanner.ml *.cmo *.cmi *.cmx *.o && \
	rm -rf _build && \
	cd ../ && \
	rm -rf $(BUILDDIR) ./oscar && \
	echo 'Oscar Build Cleaned'

clean_library :
	@echo 'Cleaning Oscar Library'
	@rm -rf /usr/local/include/oscar && \
	echo 'Oscar Library Cleaned'