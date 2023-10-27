.PHONY : all
all : build

.PHONY : phony
phony :

.PHONY : build
build :
	@ eval $(opam env) && dune build

.PHONY : test
test :
	@ eval $(opam env) && dune exec test/test.exe -- test
test-% : phony
	@ eval $(opam env) && dune exec test/test.exe -- test $*

.PHONY : top
top :
	@ eval $(opam env) && dune utop . -- -init top.ml

.PHONY : clean
clean :
	@ eval $(opam env) && dune clean
