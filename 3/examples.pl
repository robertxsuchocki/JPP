wyborWierzcholka([v0, a, v1, v3], B).
wyborWierzcholka([v0, e, v1, v3], B).

usunWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).
usunWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v2, a]).
usunWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, [v3, a]).

jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v0, a, v1, v3],[v1, e, v2],[v2, a],[v3, a]]).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).
jestWyborem([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a], [v4, e]], [[v1, e, v2],[v0, a, v1, v3],[v2, a],[v3, a]]).

wezDowolnaNazwe([v0, v1, v2, v3, v4], B, C).

znajdzWierzcholek([[v0, a, v1, v3],[v1, e, v2, v3],[v2, a],[v3, a]], B, C).

jestDFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
jestDFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
jestDFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).

jestADFS([[v1, e, v2, v3], [v2, a], [v3, a]], B).
jestADFS([[v0, a, v1, v3], [v1, e, v2], [v2, a], [v3, a]], B).
jestADFS([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).

jestADFS1([[v0, a, v1, v3], [v1, e, v2, v3], [v2, a], [v3, a]], B).
