"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Axiom
	instanceVariableNames: 'symbol'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Axiom guid: (GUID fromString: '{A6A3289F-3B9F-4799-98CF-28EF45AE4E7C}')!
Axiom comment: ''!
!Axiom categoriesForClass!Kernel-Objects! !
!Axiom methodsFor!

= axiom
	"Compares two axiom objects.
	Two axioms are equal if they're both terminal/nonterminal
	and if their symbols match"
	(axiom isNil) ifTrue: [^false].
	^(self class = axiom class) & (self symbol = axiom symbol)!

hash
	"Hash rule that needed to be overridden, because of = method changes"
	^self class hash bitXor: self symbol hash!

isNonterminal
	"Answers if axiom is a nonterminal, overridden in Nonterminal to return true"
	^false!

isTerminal
	"Answers if axiom is a terminal, overridden in Terminal to return true"
	^false!

symbol
	^symbol!

symbol: s
	"Initializes symbol with axiom name"
	symbol := s! !
!Axiom categoriesFor: #=!public! !
!Axiom categoriesFor: #hash!public! !
!Axiom categoriesFor: #isNonterminal!private! !
!Axiom categoriesFor: #isTerminal!private! !
!Axiom categoriesFor: #symbol!private! !
!Axiom categoriesFor: #symbol:!private! !

!Axiom class methodsFor!

symbol: s
	"Generates new object with symbol as axiom name"
	^self new symbol: s! !
!Axiom class categoriesFor: #symbol:!public! !

