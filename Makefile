TARGET=oscar.native
OSCAR=oscar
STDLIB=stdlib.oscar
NATIVE=native
ACTOR=$(NATIVE)/actor
IMMUT=$(NATIVE)/immut

BUILDDIR=include

oscar : library
	@echo 'Buliding Oscar Compiler'
	@cd ./src && \
	eval `opam config env` && \
	make && \
	cd ../ && \
	mkdir -p $(BUILDDIR) && \
	mv ./src/$(TARGET) ./$(OSCAR) && \
	cp ./src/$(STDLIB) ./$(BUILDDIR) && \
	cp ./src/$(IMMUT)/* ./$(BUILDDIR) && \
	cp ./src/$(ACTOR)/* ./$(BUILDDIR)

library : clean_library
	@echo 'Building Oscar Library'
	@sudo mkdir /usr/local/include/oscar && \
	sudo cp src/native/immut/*.hpp  /usr/local/include/oscar/ && \
	sudo cp src/native/actor/*.hpp  /usr/local/include/oscar/

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
	sudo rm -f oscar parser.ml parser.mli scanner.ml *.cmo *.cmi *.cmx *.o && \
	sudo rm -rf _build && \
	cd ../ && \
	sudo rm -rf $(BUILDDIR) ./oscar

clean_library :
	@echo 'Cleaning Oscar Library'
	@sudo rm -rf /usr/local/include/oscar
