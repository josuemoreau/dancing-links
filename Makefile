langford-pairing: dlx.cmx langford_pairing.cmx
	ocamlopt -o langford-pairing $^

test: dlx.cmx test.cmx
	ocamlopt -o test $^

.PRECIOUS: %.cmi
.DEFAULT_GOAL := langford-pairing

.SUFFIXES: .ml .mli .cmi .cmx

.mli.cmi:
	ocamlopt -c $<

.ml.cmx:
	ocamlopt -c $<

clean:
	rm -f *.cmi *.cmx *.o
	rm -f langford-pairing
	rm -f test
	rm -f .depend

.depend:
	ocamldep *.ml *.mli > .depend

include .depend
