all : lambda_calcul

lambda_calcul: lambda.ml
	ocamlbuild -use-ocamlfind lambda.native

zipper.native: zipper.ml
	ocamlbuild -use-ocamlfind zipper.native


clean: 
	ocamlbuild -clean
	rm reponse_serv.txt
