jestWyborem([[v0,e,v1,v3],[v1,e,v2,v3],[v2,a,v3],[v3,a]],X).
X = [[v0,e,v1],[v1,e,v2],[v2,a,v3],[v3,a]] ? ;
X = [[v0,e,v1],[v1,e,v3],[v2,a,v3],[v3,a]] ? ;
X = [[v0,e,v3],[v1,e,v2],[v2,a,v3],[v3,a]] ? ;
X = [[v0,e,v3],[v1,e,v3],[v2,a,v3],[v3,a]] ? ;

jestWyborem([[v0,a,v1,v3],[v1,e,v2,v3],[v2,a],[v3,a]],[[v3,a],[v2,a],[v1,e,v2],[v0,a,v1,v3]]).
yes

jestWyborem([[v0,a,v1,v3],[v1,e,v2,v3],[v2,a],[v3,a],[v4,e]],[[v1,e,v2],[v0,a,v1,v3],[v2,a],[v3,a]]).
no

jestDFS([[v1,e,v2,v3],[v2,a],[v3,a]],X).
X = [v1,v2,v3] ? ;
X = [v1,v3,v2] ? ;

jestDFS([[v0,a,v1,v3],[v1,e,v2],[v2,a],[v3,a]],X).
X = [v0,v1,v2,v3] ? ;
X = [v0,v3,v1,v2] ? ;

jestDFS([[v0,a,v1,v3],[v1,e,v2,v3],[v2,a],[v3,a]],X).
X = [v0,v1,v2,v3] ? ;
X = [v0,v1,v3,v2] ? ;
X = [v0,v3,v1,v2] ? ;

jestADFS([[v1,e,v2,v3],[v2,a],[v3,a]],X).
X = [v1,v2] ? ;
X = [v1,v3] ? ;
jestADFS1([[v1,e,v2,v3],[v2,a],[v3,a]],X).
X = [v1,v2] ? ;
X = [v1,v3] ? ;

jestADFS([[v0,a,v1,v3],[v1,e,v2],[v2,a],[v3,a]],X).
X = [v0,v1,v2,v3] ? ;
X = [v0,v3,v1,v2] ? ;
jestADFS1([[v0,a,v1,v3],[v1,e,v2],[v2,a],[v3,a]],X).
X = [v0,v1,v2,v3] ? ;
X = [v0,v3,v1,v2] ? ;

jestADFS([[v0,a,v1,v3],[v1,e,v2,v3],[v2,a],[v3,a]],X).
X = [v0,v1,v2,v3] ? ;
X = [v0,v3,v1,v2] ? ;
X = [v0,v1,v3] ? ;
X = [v0,v3,v1] ? ;
jestADFS1([[v0,a,v1,v3],[v1,e,v2,v3],[v2,a],[v3,a]],X).
X = [v0,v1,v2,v3] ? ;
X = [v0,v1,v3] ? ;
X = [v0,v3,v1] ? ;
X = [v0,v3,v1,v2] ? ;

jestADFS([[v0,a,v1,v3,v4],[v1,e,v2,v3,v4],[v2,a],[v3,a],[v4,a]],X).
X = [v0,v1,v2,v3,v4] ? ;
X = [v0,v1,v2,v4,v3] ? ;
X = [v0,v3,v1,v2,v4] ? ;
X = [v0,v3,v4,v1,v2] ? ;
X = [v0,v4,v1,v2,v3] ? ;
X = [v0,v4,v3,v1,v2] ? ;
X = [v0,v1,v3,v4] ? ;
X = [v0,v3,v1,v4] ? ;
X = [v0,v3,v4,v1] ? ;
X = [v0,v4,v1,v3] ? ;
X = [v0,v4,v3,v1] ? ;
X = [v0,v1,v4,v3] ? ;
X = [v0,v3,v1,v4] ? ;
X = [v0,v3,v4,v1] ? ;
X = [v0,v4,v1,v3] ? ;
X = [v0,v4,v3,v1] ? ;
jestADFS1([[v0,a,v1,v3,v4],[v1,e,v2,v3,v4],[v2,a],[v3,a],[v4,a]],X).
X = [v0,v1,v2,v3,v4] ? ;
X = [v0,v1,v2,v4,v3] ? ;
X = [v0,v1,v3,v4] ? ;
X = [v0,v1,v4,v3] ? ;
X = [v0,v3,v1,v4] ? ;
X = [v0,v3,v1,v2,v4] ? ;
X = [v0,v3,v1,v4] ? ;
X = [v0,v3,v4,v1] ? ;
X = [v0,v3,v4,v1] ? ;
X = [v0,v3,v4,v1,v2] ? ;
X = [v0,v4,v1,v3] ? ;
X = [v0,v4,v1,v2,v3] ? ;
X = [v0,v4,v1,v3] ? ;
X = [v0,v4,v3,v1] ? ;
X = [v0,v4,v3,v1] ? ;
X = [v0,v4,v3,v1,v2] ? ;
