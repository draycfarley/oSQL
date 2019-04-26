#This was dervied from A2/A3. Credit to Spring 2019 CS 3110 course staff 
MODULES=main database parser io
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=str
OCAMLPLAY= ocamlbuild -use-ocamlfind -I _build -package $(PKGS) 

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

db:
	$(OCAMLPLAY) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip sprint.zip *.ml* *.json _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adv.zip
