all : custom.cmxs

custom.cmxs : custom.ml
	ocamlopt -I +ocamldoc -shared -o custom.cmxs custom.ml

custom.cmo : custom.ml
	ocamlc -I +ocamldoc -c custom.ml

clean :
	rm -f *.cmi *.cmo *.cmx *.cmxs *.o *.bak
