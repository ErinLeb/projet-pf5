.PHONY : all
all : build

.PHONY : build
build :
	@ eval $(opam env) && dune build

.PHONY : top
top :
	@ eval $(opam env) && dune utop . -- -init top.ml

.PHONY : clean
clean :
	@ eval $(opam env) && dune clean
