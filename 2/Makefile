all: bnfc
	happy -gcai ParBreve.y
	alex -g LexBreve.x
	/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/ghc --make interpreter.hs -o interpreter

bnfc: Breve.cf
	/home/students/inf/PUBLIC/MRJP/bin/bnfc Breve.cf

clean:
	rm Abs* Doc* Err* Lex* Par* Print* Skel* Test*
	rm interpreter interpreter.hi interpreter.o TypeBreve.hi TypeBreve.o

