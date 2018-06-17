| package |
package := Package name: 'rs361086'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Axiom;
	add: #Nonterminal;
	add: #ParserLL1;
	add: #Rule;
	add: #Terminal;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\..\Users\rober\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Axiom
	instanceVariableNames: 'symbol'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ParserLL1
	instanceVariableNames: 'start rules'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Rule
	instanceVariableNames: 'left right'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Axiom subclass: #Nonterminal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Axiom subclass: #Terminal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

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

ParserLL1 guid: (GUID fromString: '{2EF7CE94-83D7-454F-B81F-75F2CE6FA80D}')!
ParserLL1 comment: ''!
!ParserLL1 categoriesForClass!Kernel-Objects! !
!ParserLL1 methodsFor!

accept
	^(self nullable) includes: start!

getNullNontermRules: r
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

printOn: stream
	rules printOn: stream!

react: x
	|react_r curr_r null_r next_r const_r|
	const_r := rules reject: [:e | (e left) symbol isNil].
	curr_r := rules select: [:e | (e left) symbol isNil].
	null_r := (self getNullNontermRules: curr_r).
	react_r := (self getReactRules: x).
	next_r := (null_r collect: [:e | e react: react_r]) select: [:e | e notNil].
	^(ParserLL1 startRaw: start rulesRaw: (next_r, const_r))!

reject
	^((self terminating) includes: start) not!

rules
	^rules!

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
!ParserLL1 categoriesFor: #getNullNontermRules:!private! !
!ParserLL1 categoriesFor: #getReactRules:!private! !
!ParserLL1 categoriesFor: #nullable!private! !
!ParserLL1 categoriesFor: #predict!public! !
!ParserLL1 categoriesFor: #printOn:!public! !
!ParserLL1 categoriesFor: #react:!public! !
!ParserLL1 categoriesFor: #reject!public! !
!ParserLL1 categoriesFor: #rules!private! !
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
	|new_r|
	new_r := self objectify: r start: s.
	(self isLL1: new_r)
		ifTrue: [^self new start: (Nonterminal symbol: nil) rules: new_r]
		ifFalse: [^nil]!

startRaw: s rulesRaw: r
	^self new start: s rules: r! !
!ParserLL1 class categoriesFor: #deterministic:!private! !
!ParserLL1 class categoriesFor: #isLL1:!private! !
!ParserLL1 class categoriesFor: #nonRecursive:!private! !
!ParserLL1 class categoriesFor: #objectify:start:!private! !
!ParserLL1 class categoriesFor: #start:rules:!public! !
!ParserLL1 class categoriesFor: #startRaw:rulesRaw:!private! !

Rule guid: (GUID fromString: '{D4616A38-6754-4D6D-86F5-F8FF7F9BF62B}')!
Rule comment: ''!
!Rule categoriesForClass!Kernel-Objects! !
!Rule methodsFor!

hasEpsilon
	^right isEmpty!

hasNonterminals
	right do: [:e | (e isNonterminal) ifTrue: [^true]].
	^false!

hasTerminals
	right do: [:e | (e isTerminal) ifTrue: [^true]].
	^false!

left
	^left!

left: l right: r
	left := l.
	right := r!

leftRecursive
	(right isEmpty) ifTrue: [^false].
	(left ~= (right at: 1)) ifTrue: [^false].
	^true!

nondeterministic: r
	|new_r similar|
	new_r := r reject: [:e | e = self].
	similar := new_r detect: [:other | (self left = other left) & ((self right: 1) = (other right: 1))] ifNone: [].
	similar isNil ifTrue: [^false].
	^true!

printOn: stream
	left printOn: stream.
	right printOn: stream!

react: r
	|rule new_right|
	rule := (r select: [:e | (e left) = (self right: 1)]) at: 1 ifAbsent: [^nil].
	new_right := (rule right, ((self right) copyFrom: 2)).
	^(Rule left: (self left) right: new_right)!

right
	^right!

right: i
	^right at: i ifAbsent: []! !
!Rule categoriesFor: #hasEpsilon!private! !
!Rule categoriesFor: #hasNonterminals!private! !
!Rule categoriesFor: #hasTerminals!private! !
!Rule categoriesFor: #left!private! !
!Rule categoriesFor: #left:right:!private! !
!Rule categoriesFor: #leftRecursive!private! !
!Rule categoriesFor: #nondeterministic:!private! !
!Rule categoriesFor: #printOn:!public! !
!Rule categoriesFor: #react:!private! !
!Rule categoriesFor: #right!private! !
!Rule categoriesFor: #right:!private! !

!Rule class methodsFor!

left: l right: r
	^self new left: l right: r! !
!Rule class categoriesFor: #left:right:!public! !

Nonterminal guid: (GUID fromString: '{A77B35DB-8CC4-425E-8E36-D6F3DCDCAE7F}')!
Nonterminal comment: ''!
!Nonterminal categoriesForClass!Kernel-Objects! !
!Nonterminal methodsFor!

isNonterminal
	^true! !
!Nonterminal categoriesFor: #isNonterminal!private! !

Terminal guid: (GUID fromString: '{51D4E92C-1E17-40B5-A4BF-B190FC369EDE}')!
Terminal comment: ''!
!Terminal categoriesForClass!Kernel-Objects! !
!Terminal methodsFor!

isTerminal
	^true! !
!Terminal categoriesFor: #isTerminal!private! !

"Binary Globals"!

