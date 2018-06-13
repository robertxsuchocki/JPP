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

start: s rules: r
        start := s.
        rules := r! !
!ParserLL1 categoriesFor: #start:rules:!public! !

!ParserLL1 class methodsFor!

isLL: r
	^(self noRecursive: r) & (self noChoices: r)!

noChoices: r
	|f|
	[r notEmpty] whileTrue: [f := r removeFirst. (r detect: [:s | (f key = s key) & ((f value at: 1 ifAbsent: []) = (s value at: 1 ifAbsent: []))] ifNone: []) notNil ifTrue: [^false]].
	^true
!

noRecursive: r
	r do: [:e | e key = (e value at: 1 ifAbsent: []) ifTrue: [^false]].
	^true
!

start: s rules: r
	(self isLL: r)
		ifTrue: [^self new start: s rules: r]
		ifFalse: [^nil]! !
!ParserLL1 class categoriesFor: #isLL:!private! !
!ParserLL1 class categoriesFor: #noChoices:!private! !
!ParserLL1 class categoriesFor: #noRecursive:!private! !
!ParserLL1 class categoriesFor: #start:rules:!public! !

