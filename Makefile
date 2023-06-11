langford-pairing: dlx.cmx langford_pairing.cmx
	ocamlopt -o langford-pairing $^

test: dlx.cmx test.cmx
	ocamlopt -o test $^

nqueens: dlx.cmx nqueens.cmx
	ocamlopt -o nqueens $^

nqueens-effects: dlx_effects.cmx nqueens_effects.cmx
	ocamlopt -I +unix unix.cmxa -o nqueens $^

.PRECIOUS: %.cmi
.DEFAULT_GOAL := langford-pairing

.SUFFIXES: .ml .mli .cmi .cmx

.mli.cmi:
	ocamlopt -c $<

.ml.cmx:
	ocamlopt -I +unix -c $<

clean:
	rm -f *.cmi *.cmx *.o
	rm -f langford-pairing test nqueens
	rm -f .depend

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
