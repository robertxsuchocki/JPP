all: bnfc
	happy -gcai ParBreve.y
	alex -g LexBreve.x
	ghc --make Breve.hs -o Breve

bnfc: Breve.cf
	bnfc Breve.cf

clean:
	-rm Abs* Breve Breve.hi Breve.o Doc* Err* Lex* Par* Print* Skel* Test*
