all: bnfc
	happy -gcai ParBreve.y
	alex -g LexBreve.x
	ghc --make interpreter.hs -o interpreter

bnfc: Breve.cf
	bnfc Breve.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm interpreter interpreter.hi interpreter.o TypeBreve.hi TypeBreve.o

