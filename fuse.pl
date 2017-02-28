
color([C,_],C).
num([_,N],N).

defused('{cn=cn, w#=w#}', A,B,C,D) :-
    color(A, C1),
    color(B, C1),
    num(C, N1),
    num(D, N1).

defused('{w5,yn,g2}', A,B,C) :-
    num(A, 5),
    color(B, yellow),
    num(C, 2),
    color(C, green).

defused('{[c/#]=[c/#]=[c/#]}', A,B,C) :-
    ((color(A, C1), color(B, C1));
     (num(A, N1), num(B, N1))),
    ((color(B, C2), color(C, C2));
     (num(B, N2), num(C, N2))).

