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

accept
	^(self nullable) includes: start!

nullable
	|new_r eps_r eps new_eps|
	new_r := rules reject: [:e | e hasTerminals].
	eps_r := new_r select: [:e | e hasEpsilon].
	eps := Set new.
	new_eps := eps_r collect: [:e | e left].
	[new_eps notEmpty] whileTrue: [
		new_eps do: [:e | eps add: e].
		eps_r := new_r select: [:e | (e right reject: [:f | eps includes: f]) isEmpty].
		new_r := new_r reject: [:e | (e right reject: [:f | eps includes: f]) isEmpty].
		new_eps := eps_r collect: [:e | e left]
	].
	^eps!

predict
	|new_r ready visited terms nullable|
	new_r := rules deepCopy.
	ready := Set with: start.
	visited := Set new.
	terms := Set new.
	nullable := self nullable.
	[ready notEmpty] whileTrue: [
		|next next_r valid|
		next := (ready asArray) first.
		next_r := new_r select: [:e | e left = next].
		next_r do: [:e |
			valid := true.
			(e right) do: [:f |
				(f isNonterminal) & ((nullable includes: f) not) ifTrue: [valid := false].
				valid ifTrue: [
					(f isNonterminal) ifTrue: [ready add: f].
					(f isTerminal) ifTrue: [terms add: f]
				].
				(f isTerminal) ifTrue: [valid := false].
			].
		].
		ready remove: next.
		visited add: next
	].
	^(terms collect: [:e | e symbol]) asOrderedCollection!

printOn: stream
	rules printOn: stream!

react: x
	|new_r ready visited new_ready|
	new_r := rules deepCopy.
	ready := Set with: (Rule left: (Terminal symbol: x) right: #()).
	visited := Set new.
	[ready notEmpty] whileTrue: [
		new_ready := (new_r collect: [:e | (e react: (ready asArray))]) select: [:e | e notNil].
		new_r := new_r reject: [:e | (e react: (ready asArray)) notNil].
		ready do: [:e | visited add: e].
		ready := new_ready
	].
	^visited asArray!

reject
	^((self terminating) includes: start) not!

start: s rules: r
        start := s.
        rules := r!

terminating
	|new_r term_r term new_term|
	new_r := rules deepCopy.
	term_r := new_r reject: [:e | e hasNonterminals].
	term := Set new.
	new_term := term_r collect: [:e | e left].
	[new_term notEmpty] whileTrue: [
		new_term do: [:e | term add: e].
		term_r := new_r select: [:e | (e right reject: [:f | (f isTerminal) | (term includes: f)]) isEmpty].
		new_r := new_r reject: [:e | (e right reject: [:f | (f isTerminal) | (term includes: f)]) isEmpty].
		new_term := term_r collect: [:e | e left]
	].
	^term! !
!ParserLL1 categoriesFor: #accept!public! !
!ParserLL1 categoriesFor: #nullable!private! !
!ParserLL1 categoriesFor: #predict!public! !
!ParserLL1 categoriesFor: #printOn:!public! !
!ParserLL1 categoriesFor: #react:!public! !
!ParserLL1 categoriesFor: #reject!public! !
!ParserLL1 categoriesFor: #start:rules:!private! !
!ParserLL1 categoriesFor: #terminating!private! !

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

