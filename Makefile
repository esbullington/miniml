
build:
	ocamlbuild -use-ocamlfind main.native

clean:
	rm -rf _build; rm main.native
