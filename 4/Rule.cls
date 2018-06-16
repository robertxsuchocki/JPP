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

