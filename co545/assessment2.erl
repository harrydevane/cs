-module(assessment2).
-export([pos_bids/1, success/2, winners/2, init/2, drop/2, subst/3, isxwin/1, linexwin/1, pick/2, wincol/1]).

pos_bids([]) -> [];
pos_bids([{Name, Amount} | Bids]) when Amount > 0 -> [{Name, Amount} | pos_bids(Bids)];
pos_bids([{_Name, _Amount} | Bids]) -> pos_bids(Bids).

success([], Threshold) -> Threshold =< 0;
success([{_Name, Amount} | Bids], Threshold) -> success(Bids, Threshold - Amount).

winners([], _Threshold) -> [];
winners([{Name, Amount} | Bids], Threshold) when Threshold > Amount -> [{Name, Amount} | winners(Bids, Threshold - Amount)];
winners([{Name, Amount} | _Bids], Threshold) when Threshold =< Amount -> [{Name, Threshold}].

init(St1, St2) -> St1 == string:slice(St2, 0, string:len(St1)).

drop(N, St) -> string:slice(St, N, string:len(St)).

subst(_Old, _New, "") -> "";
subst(Old, New, St) -> 
    case init(Old, St) of
        true -> New ++ drop(string:len(Old), St);
        false -> string:slice(St, 0, 1) ++ subst(Old, New, drop(1, St))
    end.

isxwin([x, x, x]) -> true;
isxwin([_, _, _]) -> false.

linexwin([[X1, X2, X3], [Y1, Y2, Y3], [Z1, Z2, Z3]]) -> isxwin([X1, X2, X3]) or isxwin([Y1, Y2, Y3]) or isxwin([Z1, Z2, Z3]).

pick(N, [_X | Xs]) when N > 0 -> pick(N - 1, Xs);
pick(_N, [X | _Xs]) -> X.

iswin([x, x, x]) -> true;
iswin([o, o, o]) -> true;
iswin([_, _, _]) -> false.

wincol([[X1, X2, X3], [Y1, Y2, Y3], [Z1, Z2, Z3]]) -> iswin([X1, Y1, Z1]) or iswin([X2, Y2, Z2]) or iswin([X3, Y3, Z3]).
