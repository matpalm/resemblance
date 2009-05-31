-module(coeff).
-compile(export_all).

jaccard(L1,L2) ->
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    IntersectionSize = sets:size(sets:intersection(S1,S2)),
    UnionSize = sets:size(sets:union(S1,S2)),
    IntersectionSize / UnionSize.
    
