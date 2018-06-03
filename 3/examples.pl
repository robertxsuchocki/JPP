choiseOfNode([v0, a, v1, v3], B).
B = [v0,a,v1,v3] ? ;

choiseOfNode([v0, e, v1, v3], B).
B = [v0,e,v1] ? ;
B = [v0,e,v3] ? ;


matchNodes([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).
B = [[v1,e,v2,v3],[v2,a],[v3,a]],
C = [v0,a,v1,v3] ? ;

matchNodes([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v2, a]).
B = [[v0,a,v1,v3],[v1,e,v2,v3],[v3,a]] ? ;

matchNodes([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v3, a]).
B = [[v0,a,v1,v3],[v1,e,v2,v3],[v2,a]] ? ;

jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B).
B = [[v0,a,v1,v3],[v1,e,v2],[v2,a],[v3,a]] ? ;
B = [[v0,a,v1,v3],[v1,e,v3],[v2,a],[v3,a]] ? ;


jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v0, a, v1, v3],[v1, e, v2],[v2, a],[v3, a]]).
yes

jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).
yes

jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a], [v4, e]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).
no


takeAnyName([v0, v1, v2, v3, v4], B, C).
B = [v1,v2,v3,v4],
C = v0 ? ;
B = [v0,v2,v3,v4],
C = v1 ? ;
B = [v0,v1,v3,v4],
C = v2 ? ;
B = [v0,v1,v2,v4],
C = v3 ? ;
B = [v0,v1,v2,v3],
C = v4 ? ;


findNode([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).
B = v0,
C = [v0,a,v1,v3] ? ;
B = v1,
C = [v1,e,v2,v3] ? ;
B = v2,
C = [v2,a] ? ;
B = v3,
C = [v3,a] ? ;


jestDFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
B = [v1,v2,v3] ? ;
B = [v1,v3,v2] ? ;

jestDFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
B = [v0,v1,v2,v3] ? ;
B = [v0,v3,v1,v2] ? ;

jestDFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).
B = [v0,v1,v2,v3] ? ;
B = [v0,v1,v3,v2] ? ;
B = [v0,v3,v1,v2] ? ;
B = [v0,v3,v1,v2] ? ;


jestADFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
B = [v1,v2] ? ;
B = [v1,v3] ? ;

jestADFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
B = [v0,v1,v2,v3] ? ;
B = [v0,v3,v1,v2] ? ;

jestADFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).
B = [v0,v1,v2,v3] ? ;
B = [v0,v3,v1,v2] ? ;
B = [v0,v1,v3] ? ;
B = [v0,v3,v1] ? ;


jestADFS1([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).
B = [v0,v1,v2,v3] ? ;
B = [v0,v1,v3] ? ;
B = [v0,v3,v1,v2] ? ;
B = [v0,v3,v1] ? ;
