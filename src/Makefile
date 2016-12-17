OCAMLBUILD = corebuild
CFLAGS = -safe-string
BUILDFLAGS = -use-ocamlfind -r -cflags $(CFLAGS)

oscar :
		$(OCAMLBUILD) $(BUILDFLAGS) oscar.native

.PHONY: clean
clean:
		$(OCAMLBUILD) -clean
