"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Rule
	instanceVariableNames: 'left right'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Rule guid: (GUID fromString: '{D4616A38-6754-4D6D-86F5-F8FF7F9BF62B}')!
Rule comment: ''!
!Rule categoriesForClass!Kernel-Objects! !
!Rule methodsFor!

hasEpsilon
	"Answers if right side represents empty string"
	^(self right) isEmpty!

hasNonterminals
	"Answers if right side of rule contains any amount of nonterminals"
	(self right) do: [:e | (e isNonterminal) ifTrue: [^true]].
	^false!

hasTerminals
	"Answers if right side of rule contains any amount of terminals"
	(self right) do: [:e | (e isTerminal) ifTrue: [^true]].
	^false!

left
	^left!

left: l right: r
	"Initializes both left and right side of production with ready objects"
	left := l.
	right := r!

leftRecursive
	"Answers if rule is left recursive.
	Checks if left side of production is equal to first axiom in right side,
	returns true if above is true and false otherwise"
	^((self left) = (self right: 1))!

nondeterministic: r
	"Answers if rule is nondeterministic given the set of rules r.
	Searches for rule that matches left side of production and
	first axiom in right side, returns true if finds any, false otherwise"
	|new_r similar|
	new_r := r reject: [:e | e = self].
	similar := new_r detect: [:other | (self left = other left) & ((self right: 1) = (other right: 1))] ifNone: [].
	^(similar notNil)!

react: r
	"Returns a new rule that is a result of it being changed by any rule in r.
	Searches for any rule in r that has its left side equal to the first axiom
	in the right side of self rule and applies a change (which means that
	first axiom in right side is replaced by a right side of a rule found in r)
	Returns changed rule if change was applied and nil if no rules fit"
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
!Rule categoriesFor: #react:!private! !
!Rule categoriesFor: #right!private! !
!Rule categoriesFor: #right:!private! !

!Rule class methodsFor!

left: l right: r
	"Generates rule based on ready objects"
	^self new left: l right: r! !
!Rule class categoriesFor: #left:right:!private! !

