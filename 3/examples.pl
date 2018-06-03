choiseOfNode([v0, a, v1, v3], B).
choiseOfNode([v0, e, v1, v3], B).

matchNodes([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).
matchNodes([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v2, a]).
matchNodes([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v3, a]).

jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v0, a, v1, v3],[v1, e, v2],[v2, a],[v3, a]]).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a], [v4, e]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).

takeAnyName([v0, v1, v2, v3, v4], B, C).

findNode([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).

jestDFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
jestDFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
jestDFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).

jestADFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
jestADFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
jestADFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).

jestADFS1([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).
