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

right: i
	^right at: i ifAbsent: []! !
!Rule categoriesFor: #left!private! !
!Rule categoriesFor: #left:right:!private! !
!Rule categoriesFor: #leftRecursive!private! !
!Rule categoriesFor: #nondeterministic:!private! !
!Rule categoriesFor: #printOn:!public! !
!Rule categoriesFor: #right:!private! !

!Rule class methodsFor!

left: l right: r
	^self new left: l right: r! !
!Rule class categoriesFor: #left:right:!public! !

