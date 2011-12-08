#ocamlbuild -use-ocamlfind test.native

ocamlbuild -use-ocamlfind \
  -tag 'package(bisect)' \
  -tag 'syntax(camlp4o)' \
  -tag 'syntax(bisect_pp)' \
  syncer.byte

#ocamlbuild -use-ocamlfind -tag 'package(ocamlviz)' \
# -tag 'pp(camlp4 pa_o.cmo str.cma pa_ocamlviz.cmo pr_o.cmo)' \
#   profile_test.native

