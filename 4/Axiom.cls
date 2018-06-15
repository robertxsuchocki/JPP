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
	(axiom isNil) ifTrue: [^false].
	^(self class = axiom class) & (self symbol = axiom symbol)!

hash
	^self class hash bitXor: self symbol hash!

isNonterminal
	^false!

isTerminal
	^false!

printOn: stream
	symbol printOn: stream!

symbol
	^symbol!

symbol: s
	symbol := s! !
!Axiom categoriesFor: #=!public! !
!Axiom categoriesFor: #hash!public! !
!Axiom categoriesFor: #isNonterminal!private! !
!Axiom categoriesFor: #isTerminal!private! !
!Axiom categoriesFor: #printOn:!public! !
!Axiom categoriesFor: #symbol!private! !
!Axiom categoriesFor: #symbol:!private! !

!Axiom class methodsFor!

symbol: s
	^self new symbol: s! !
!Axiom class categoriesFor: #symbol:!public! !

