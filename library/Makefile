COLOR = color
IMAGE = image
SOCKET = socket
WORLD = world
UNIVERSE = universe

all : bcl

bcl :
	make bcl -f Makefile-$(COLOR)
	make bcl -f Makefile-$(IMAGE)
	make bcl -f Makefile-$(SOCKET)
	make bcl -f Makefile-$(WORLD)
	make bcl -f Makefile-$(UNIVERSE)

ncl :
	make ncl -f Makefile-$(COLOR)
	make ncl -f Makefile-$(IMAGE)
	make ncl -f Makefile-$(SOCKET)
	make ncl -f Makefile-$(WORLD)
	make ncl -f Makefile-$(UNIVERSE)

htdoc : bcl ocamldoc/custom.cmxs
	make htdoc -f Makefile-htdoc PASSOPTFLAGS="-lang en"

htdoc-ja : bcl ocamldoc/custom.cmxs
	make htdoc -f Makefile-htdoc PASSOPTFLAGS="-lang ja"

ocamldoc/custom.cmxs : ocamldoc/custom.ml
	cd ocamldoc; make

clean :
	make clean -f Makefile-$(COLOR)
	make clean -f Makefile-$(IMAGE)
	make clean -f Makefile-$(SOCKET)
	make clean -f Makefile-$(WORLD)
	make clean -f Makefile-$(UNIVERSE)
	cd ocamldoc; make clean
	rm -Rf doc

install : bcl
	ocamlfind install universe META $(wildcard *.cma *.mli *.cmxa *.cmi)

uninstall :
	ocamlfind remove universe
