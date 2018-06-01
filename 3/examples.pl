jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v0, a, v1, v3],[v1, e, v2],[v2, a],[v3, a]]).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a], [v4, e]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).

usunWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).
usunWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v2, a]).
usunWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v3, a]).

znajdzWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], v0, B).

jestDFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
jestDFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
jestDFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).

jestADFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
jestADFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
jestADFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).
