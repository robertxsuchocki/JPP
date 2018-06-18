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
	"Answer if language contains empty string.
	Similarly to reject, this uses nullable method and checks if start is contained,
	which means that you can get empty string from starting nonterminal"
	^(self nullable) includes: (self start)!

getNullNontermRules: r
	"Given rules, expand them with additional rules that may occur if we
	consider all nullable nonterminals as evaluating to empty string.
	For every rule we check whether its first right rule axiom is a nullable
	nonterminal and if it's true we add a rule without it and search for additional
	shortened rules based on this shorter rule, if false we add nothing and skip"
	|new_r nullable cut_r next|
	new_r := Set new.
	nullable := (self nullable).
	r do: [ :e |
		cut_r := e right.
		[cut_r notEmpty] whileTrue: [
			next := cut_r at: 1.
			cut_r := cut_r copyFrom: 2.
			(nullable includes: next)
				ifTrue: [
					new_r add: (Rule left: (e left) right: cut_r).
				]
				ifFalse: [
					cut_r := cut_r reject: [:f|true].
				].
		]
	].
	^(r, (new_r asArray))!

getReactRules: x
	"Return rules that used on right side's first elements will reflect react changes.
	This set starts with rule that changes given terminal x to empty string,
	next it will be extended in loop by every rule that has first right side axiom
	affected by any of these rules (first right side axiom equals left side axiom)
	until it stops expanding and no more rules can be produced"
	|new_r ready visited new_ready|
	new_r := (self rules) deepCopy.
	ready := Set with: (Rule left: (Terminal symbol: x) right: #()).
	visited := Set new.
	[ready notEmpty] whileTrue: [
		new_ready := (new_r collect: [:e | (e react: (ready asArray))]) select: [:e | e notNil].
		new_r := new_r reject: [:e | (e react: (ready asArray)) notNil].
		ready do: [:e | visited add: e].
		ready := new_ready
	].
	^visited asArray!

nullable
	"Answer which nonterminals are nullable (can result in an empty string).
	Algorithm skips rules with terminals, then initializes set with nonterminals
	that result in an empty string in one rule and then extends this set in loop with
	nonterminals that have rules containing only nonterminals from nullable set
	until it doesn't extend anymore, which stops our search"
	|new_r eps_r eps new_eps|
	new_r := (self rules) reject: [:e | e hasTerminals].
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
	"Give a collection of terminals that can be next in any of the generated words.
	Algorithm starts with start, goes through all its rule's right side elements
	up until its first nonnullable nonterminal, on the way remembers all terminals
	in stored answer and nonterminals in an axiom queue ready for further check,
	then goes through all nonterminals as they can also contain first terminals of words
	and remembers its first terminals until queue's empty and there's nothing to find"
	|new_r ready visited terms nullable|
	new_r := (self rules) deepCopy.
	ready := Set with: (self start).
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
				valid ifTrue: [
					(f isNonterminal) ifTrue: [ready add: f].
					(f isTerminal) ifTrue: [terms add: f].
					(f isNonterminal) & (nullable includes: f) ifFalse: [valid := false].
				]
			]
		].
		ready remove: next.
		visited add: next
	].
	^(terms collect: [:e | e symbol]) asOrderedCollection!

react: x
	"Give a Brzozowski derivative of parser with given terminal x.
	This rule takes advantage of additionally created rules for current state
	as it separates this rules from the rest of language, then expands them
	with all possibilities given by getNullNontermRules and finally applies
	rules from getReactRules on them, retrieving current state for rules
	that got affected and removing unfitting rules by filtering out nils"
	|self_r const_r curr_r null_r react_r next_r|
	self_r := self rules.
	const_r := self_r reject: [:e | (e left) symbol isNil].
	curr_r := self_r select: [:e | (e left) symbol isNil].
	null_r := (self getNullNontermRules: curr_r).
	react_r := (self getReactRules: x).
	next_r := (null_r collect: [:e | e react: react_r]) select: [:e | e notNil].
	^(ParserLL1 startRaw: start rulesRaw: (next_r, const_r))!

reject
	"Answer if language is empty.
	This makes use of terminating method and checks whether start is excluded,
	which means that no word can be built from starting point"
	^((self terminating) includes: (self start)) not!

rules
	^rules!

start
	^start!

start: s rules: r
	"Simply initialize start axiom and rules collection
	after they're build by similar class method"
        start := s.
        rules := r!

terminating
	"Answer which nonterminals are terminating (can result in finite amount of terminals).
	Algorithm works similarly to nullable, it initializes set with nonterminals that result
	in terminal-only productions in one rule and then extends this set in loop with
	nonterminals that have rules containing nonterminals from terminating set and/or
	terminals until it doesn't extend anymore, which stops our search"
	|new_r term_r term new_term|
	new_r := (self rules) deepCopy.
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
!ParserLL1 categoriesFor: #getNullNontermRules:!private! !
!ParserLL1 categoriesFor: #getReactRules:!private! !
!ParserLL1 categoriesFor: #nullable!private! !
!ParserLL1 categoriesFor: #predict!public! !
!ParserLL1 categoriesFor: #react:!public! !
!ParserLL1 categoriesFor: #reject!public! !
!ParserLL1 categoriesFor: #rules!private! !
!ParserLL1 categoriesFor: #start!private! !
!ParserLL1 categoriesFor: #start:rules:!private! !
!ParserLL1 categoriesFor: #terminating!private! !

!ParserLL1 class methodsFor!

deterministic: r
	"Checks if all rules in set are deterministic.
	This means there are no rule pairs like
	A -> aB
	A -> aC
	where parser would have to choose one option,
	which indicates that grammar is not LL(1)"
	r do: [:e | (e nondeterministic: r) ifTrue: [^false]].
	^true
!

isLL1: r
	"Checks if all rules in set are deterministic and non recursive.
	If both are true then given grammar is LL(1)"
	^(self nonRecursive: r) & (self deterministic: r)!

nonRecursive: r
	"Checks if every rule in set is not left recursive.
	This means there's no rule like
	A -> AB
	which would make parser run forever and
	which indicates that grammar is not LL(1)"
	r do: [:e | (e leftRecursive) ifTrue: [^false]].
	^true
!

objectify: r start: s
	"Transforms input to objects.
	This method generates Axiom and Rule objects based on input, which are later
	saved into the parser object, it also saves some additional rules, which indicate
	current parser state. These rules start with nil (as it is the only thing that will
	definitely not be a name of any axiom), which means they're the starting rules
	and in this method, they're created by copying right sides of rules for s axiom,
	but later on with multiple reacts done, s axiom rules will not be touched and
	only nil rules will be changed to indicate parser change and reflect react method"
	|nonterms new_r curr_r new_key new_value|
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
	curr_r := (new_r select: [:e | (e left) symbol = s]) collect: [:e | Rule left: (Nonterminal symbol: nil) right: (e right)].
	^(curr_r asArray, new_r asArray)!

start: s rules: r
	"Generates parser object.
	This method firstly checks whether grammar is LL(1), if it's true then
	it generates needed axiom and rule objects, including additional nil
	rules reflecting current parser state, and sets the start on nil,
	otherwise it returns nil"
	|new_r|
	new_r := self objectify: r start: s.
	(self isLL1: new_r)
		ifTrue: [^self new start: (Nonterminal symbol: nil) rules: new_r]
		ifFalse: [^nil]!

startRaw: s rulesRaw: r
	"Generates parser object based on start and rules objects.
	This method is used in react method, where we want to generate object
	based on already prepared, but slightly changed start and rule objects.
	This rule skips all the checks and just creates parser with objects given"
	^self new start: s rules: r! !
!ParserLL1 class categoriesFor: #deterministic:!private! !
!ParserLL1 class categoriesFor: #isLL1:!private! !
!ParserLL1 class categoriesFor: #nonRecursive:!private! !
!ParserLL1 class categoriesFor: #objectify:start:!private! !
!ParserLL1 class categoriesFor: #start:rules:!public! !
!ParserLL1 class categoriesFor: #startRaw:rulesRaw:!private! !

