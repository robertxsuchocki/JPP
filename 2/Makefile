all: bnfc
	happy -gcai ParBreve.y
	alex -g LexBreve.x
	/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin --make interpreter.hs -o interpreter

bnfc: breve.cf
	bnfc breve.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm interpreter interpreter.hi interpreter.o TypeBreve.hi TypeBreve.o

