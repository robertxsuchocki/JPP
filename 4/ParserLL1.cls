"Filed out from Dolphin Smalltalk 7"!

Object subclass: #ParserLL1
	instanceVariableNames: 'start rules'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ParserLL1 guid: (GUID fromString: '{2EF7CE94-83D7-454F-B81F-75F2CE6FA80D}')!
ParserLL1 comment: ''!
!ParserLL1 categoriesForClass!Kernel-Objects! !
!ParserLL1 methodsFor!

printOn: stream
	start printOn: stream.
	' => ' printOn: stream.
	rules printOn: stream!

start: s rules: r
        start := s.
        rules := r! !
!ParserLL1 categoriesFor: #printOn:!public! !
!ParserLL1 categoriesFor: #start:rules:!private! !

!ParserLL1 class methodsFor!

deterministic: r
	r do: [:e | (e nondeterministic: r) ifTrue: [^false]].
	^true
!

isLL1: r
	^(self nonRecursive: r) & (self deterministic: r)!

nonRecursive: r
	r do: [:e | (e leftRecursive) ifTrue: [^false]].
	^true
!

objectify: r start: s
	|nonterms new_r new_key new_value|
	nonterms := Set with: s.
	r do: [:e | nonterms add: e key].

	new_r := r collect: [:e | |index axiom_obj|
			index := 0.
			new_key := Nonterminal symbol: e key.
			new_value := Array new: (e value size).
			(e value) do: [:v |
				index := index + 1.
				(nonterms includes: v)
					ifTrue: [axiom_obj := Nonterminal symbol: v]
					ifFalse: [axiom_obj :=Terminal symbol: v].
				new_value at: index put: axiom_obj
			].
			Rule left: new_key right: new_value
		].
	^new_r!

start: s rules: r
	|new_r|
	new_r := self objectify: r start: s.
	(self isLL1: new_r)
		ifTrue: [^self new start: (Nonterminal symbol: s) rules: new_r]
		ifFalse: [^nil]! !
!ParserLL1 class categoriesFor: #deterministic:!private! !
!ParserLL1 class categoriesFor: #isLL1:!private! !
!ParserLL1 class categoriesFor: #nonRecursive:!private! !
!ParserLL1 class categoriesFor: #objectify:start:!private! !
!ParserLL1 class categoriesFor: #start:rules:!public! !

