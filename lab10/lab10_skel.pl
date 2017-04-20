%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Atenție la semnăturile predicatelor și la specificarea parametrilor
%% p(+Arg1, -Arg2, ?Arg3)
%%  * Arg1 va fi instanțiat atunci când se va încerca satisfacerea p/3
%%  * Arg2 se va instanția odată cu satisfacerea p/3
%%  * Arg3 va putea fi instanțiat sau nu atunci când se va satisface p/3:
%%
%% Interogați Prolog cu "checkXX." pentru a verifica rezolvarea exercițiului XX.
%% Pentru a vedea progresul general, trimiteți scopul "check".
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% 1. (1p)
%% myConcat/3
%% myConcat(?List1, ?List2, ?List)
%% 'List' este lista formată prin concatenarea listelor 'List1' și 'List2'.

myConcat(_,_,_):- fail.
%myConcat([], L2, L2):-true.
%myConcat([H | L1], L2, [H | R]) :- myConcat(L1, L2, R).

check1:-
        myConcat([], [], La), La == [], write('.'),!,
        myConcat(L1b, [1,2], [1,2]), L1b == [], write('.'),!,
        myConcat([1,2,3], L2c, [1,2,3,4,5]), L2c == [4, 5], write('.'),!,
        myConcat([a, b, c], [d], [a, b, c, d]), write('.'),!,
        myConcat([X1, X2], [X1, X2], [1, 2, 1, 2]), write('.'),!,
        \+ myConcat([X1, X2], [X1, X2], [1, 2, 3, 4]), write('.'),!,
        findall(L3, myConcat([_,_,_], [_], L3), [_]), write('.'),!,
        findall(L4, myConcat([_,_,_], L4, [_,_,_,_]), [_]), write('.'),!,
        myConcat([X51],[X52],[X53,X54]),
        X51 == X53, var(X51), X52 == X54, var(X54), write('.'),!,
        writeln('Exercițiul 1 a fost rezolvat corect!'),
        !.

%% -----------------------------------------------------------------------------

%% 2a. (0.5p)
%% myReverse/2
%% myReverse(?List, +RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă.
%% Regulile pot conține și predicatul myConcat/3.

myReverse(_,_):- fail.
%myReverse([], [], []) :- true.
%myReverse([H], [H] ):- true.
%myReverse([H | L1] , R):- myReverse(L1, P) , myConcat(P, [H], R).

check2a:-
        myReverse([], []), write('.'), !,
        myReverse([1,2,3], [3,2,1]), write('.'),!, !,
        myReverse([1,2,3], Rev3), Rev3 == [3,2,1], write('.'),
        myReverse(List4, [3,2,1]), List4 == [1,2,3], write('.'), !,
        myReverse([1,X2,X3], [3,2,X1]), X1 == 1, X2 == 2, X3 == 3, write('.'),!,
        myReverse([Y1,Y2], L5), L5 == [Y2, Y1], var(Y1), var(Y2), write('.'), !,
        myReverse(L6, [Z6]), L6 == [Z6], var(Z6), write('.'), !,
        findall(L7, myReverse([_,_], L7), [_]), write('.'), !,
        writeln('Exercițiul 2a a fost rezolvat corect!'),
        !.

%% -----------------------------------------------------------------------------

%% 2b. (0.5p)
%% myReverseAcc/2
%% myReverseAcc(?List, ?Acc, ?RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă
%% și elementele listei 'Acc'.
%% (Indicație: 'Acc' se va comporta precum un acumulator)
%% Regulile nu trebuie să conține alte predicate (în afară de "cut" și ",").


myReverseAcc(_,_,_):-fail.
%myReverseAcc( [] , Acc, Acc):-true.

%myReverseAcc(L , [], myReverse(L, R)):- true.
%myReverseAcc( [H | L] , Acc, RevList):- myReverseAcc(L, [H | Acc], RevList).
check2b:-
        myReverseAcc([], [], []), write('.'),
        myReverseAcc([1,2,3], [0], [3,2,1,0]), write('.'),
        myReverseAcc([1,2,3], [0], Rev3), Rev3 == [3,2,1,0], write('.'),
        myReverseAcc(List4, [0], [3,2,1,0]), List4 == [1,2,3], write('.'),
        myReverseAcc([X2,1], [3], [X1,2,3]), X1 == 1, X2 == 2, write('.'),
        writeln('Exercițiul 2b rezolvat corect!'),
        !.

check2:-
        check2a, check2b.

%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% 3. (1p)
%% extract/4
%% extract(+List, +Start, +End, -Range)
%% 'Range' reprezintă lista formată din elementele listei 'List' aflate
%% pe pozițiile din intervalul 'Start' .. 'End'. 'Start' va fi întotdeauna
%% mai mic decât 'End'. Primul element se află pe poziția 0. Dacă 'End'
%% depășește lungimea listei, 'Range' va conține toate elementele de la 'Start'
%% la finalul listei.

extract(_,_,_,_):- fail.
%extract(List, 0, -1, []) :- true.
%extract([], _, _, []) :- true.

%extract([H | List], Start, Last, [H : Range]) :- St2 is Start + 1, St2 < Last,
%extract(List, St2, Last, Range).
%extract([H | List], Start, Last, [H : Range]) :- St2 is Start + 1, St2 < Last, 
%extract([H : List], Start, Last, [H : Range])

check3:-
        extract([a,b,c,d,e], 0, 3, [a,b,c,d]), write('.'),
        extract([a,b,c,d,e], 1, 10, [b,c,d,e]), write('.'),
        extract([a,b,C,D,e], 2, 4, [c,d,e]), C == c, D == d, write('.'),
        extract([a,b,C1,D1,e], 2, 4, [C2,D2,e]), C1 == C2, D1 == D2, write('.'),
        extract([a,b,c,_,d], 3, 4, [X,X]), write('.'),
        writeln('Exercițiul 3 rezolvat corect!'),
        !.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% 4a. (0.5p)
%% factorial/2
%% factorial(+N, -Fact)
%% 'Fact' este factorialul lui 'N'.
%% N va fi mereu legat la un număr natural.

factorial(_,_):- fail.
%factorial(0,1):true.
%factorial(X , Fact) :- X2 is X-1 , factorial(X2, Fact2) , Fact is Fact2 * X

check4a:-
        factorial(1, F1), F1 == 1, write('.'),
        factorial(4, F2), F2 == 24, write('.'),
        factorial(5, 120), write('.'),
        factorial(6, 720), write('.'),
        factorial(7, 5040), write('.'),
        writeln('Exercițiul 4a rezolvat corect!'),
        !.
%% -----------------------------------------------------------------------------

%% 4b. (0.5p)
%% factorial2/2
%% factorial2(?N, ?Fact)
%% 'Fact' este factorialul lui 'N'.
%% Cel puțin unul dintre cele două argumente trebuie să fie legat la un număr.


%%trebuie dedus oricare dintre parametrii

factorial2(_ ,_):- fail.

check4b:-
        factorial2(2, F1), F1 == 2, write('.'),
        factorial2(4, F2), F2 == 24, write('.'),
        factorial2(N3, 120), N3 == 5, write('.'),
        factorial2(N4, 720), N4 == 6, write('.'),
        factorial2(7, 5040), write('.'),
        writeln('Exercițiul 4b rezolvat corect!'),
        !.

check4:-
        check4a,
        check4b.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% 5. (1p)
%% palindrom/2
%% palindrom(+List)
%% 'List' este un palindrom.

palindrom(_):-fail.
%palindrom(L) :- myReverse(L) = L.

check5 :-
        palindrom([1,2,3,2,1]), write('.'),
        palindrom([1,2,3,3,2,1]), write('.'),
        \+ palindrom([1,2,3,0,2,1]), write('.'),
        palindrom([1,2,3,X3,X2,X1]), X3 == 3, X2 == 2, X1 == 1, write('.'),
        \+ palindrom([1,2,3,X,_,X]), write('.'),
        writeln('Exercițiul 5 rezolvat corect!'),
        !.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% 6. (1p)
%% sublista/2
%% sublista(+List, ?SubList)
%% 'SubList' este o sublistă a lui 'List' ('SubList' poate fi obținută prin
%% eliminarea a zero sau mai multe elemente din 'List'

sublista(_,_):-fail.

check6 :-
        sublista([1,2,3,4,5],[2,3,4]), write('.'),
        sublista([1,2,3,4,5],[1,2,3,4,5]), write('.'),
        \+ sublista([1,2,3,4,5],[2,1,4]), write('.'),
        findall(S4, sublista([1,2,3], S4), All4),
        sort(All4, [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]), write('.'),
        sublista([X1,X2,X3],[Y1,Y2,Y3]), X1==Y1, X2==Y2, X3==Y3, write('.'),
        writeln('Exercițiul 6 rezolvat corect!'),
        !.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Se dau următoarele fapte ce descriu arcele unei păduri de arbori binari.
%% Fiecare nod poate avea maxim doi fii.

nod(a). nod(b). nod(c). nod(d). nod(e). nod(f). nod(g).
nod(h). nod(i). nod(j). nod(k). nod(l).
nod(m).
nod(n). nod(p). nod(o).

%% -----------------------------------------------------------------------------
%% arc/2

arc(a,b). arc(a,c). arc(b,d). arc(c,e). arc(c,g). arc(e,f).
arc(h,i). arc(h,j). arc(i,k). arc(j,l).
arc(n,o). arc(o,p).

%% -----------------------------------------------------------------------------
%% 7a. (0.5p)
%% isLeaf/1
%% isLeaf(?Nod)

isLeaf(_):- fail.

check7a:-
        isLeaf(d), isLeaf(f), isLeaf(g), isLeaf(k), isLeaf(l), write('.'), !,
        isLeaf(m), isLeaf(p), write('.'), !,
        \+ isLeaf(a), \+ isLeaf(b), \+ isLeaf(c), \+ isLeaf(e), write('.'), !,
        findall(X, isLeaf(X), L), length(L, 7), write('.'), !,
        writeln('Exercițiul 7a rezolvat corect!'),
        !.



%% -----------------------------------------------------------------------------
%% 7b. (0.5p)
%% isRoot/1

isRoot(_):- fail.

check7b:-
        isRoot(a), isRoot(h), isRoot(m), isRoot(n), write('.'), !,
        \+ isRoot(b), \+ isRoot(d), \+ isRoot(e), \+ isRoot(l), write('.'), !,
        findall(X, isRoot(X), L), length(L, 4), write('.'), !,
        writeln('Exercițiul 7b rezolvat corect!'),
        !.


%% -----------------------------------------------------------------------------
%% 7c. (1p)
%% descendantOf/2
%% descendantOf(?X,?Y)
%% Nodul X este un urmaș a lui Y.

descendantOf(_,_):-fail.

check7c:-
        descendantOf(b,a), descendantOf(c,a), descendantOf(d,b),
        descendantOf(j,h), descendantOf(l,j), write('.'), !,
        descendantOf(j,h), descendantOf(f,a), write('.'), !,
        \+ descendantOf(a,_), \+ descendantOf(h,_), \+ descendantOf(m,_),
        write('.'), !,
        findall(X1, descendantOf(X1,a), L1), length(L1, 6), write('.'), !,
        findall(X2, descendantOf(X2,h), L2), length(L2, 4), write('.'), !,
        findall(X3, descendantOf(l,X3), L3), length(L3, 2), write('.'), !,
        findall(X4, descendantOf(f,X4), L4), length(L4, 3), write('.'), !,
        writeln('Exercițiul 7c rezolvat corect!'),
        !.

%% -----------------------------------------------------------------------------
%% 7d. (2p)
%% descendants/2
%% descendants(?Nod, ?N)
%% Nodul Nod are N urmași.

descendants(_,_):-fail.

check7d:-
        descendants(a, 6), descendants(b, 1), write('.'), !,
        descendants(a, N1), N1 == 6, write('.'), !,
        descendants(c, 3), descendants(h, 4), write('.'), !,
        descendants(g, 0), descendants(l, 0), write('.'), !,
        findall(X1, descendants(X1, 1), L), sort(L, [b,e,i,j,o]), write('.'), !,
        findall(X2, descendants(X2, 2), L2), sort(L2, [n]), write('.'), !,
        writeln('Exercițiul 7d rezolvat corect!'),
        !.


%% -----------------------------------------------------------------------------
%% 7e. (bonus: 1p)
%% height/2
%% height(?Nod, ?H).

height(_, _):- fail.

check7e:-
        height(a,N1), N1 == 3, write('.'), !,
        height(a,3), height(b,1), height(m,0), write('.'), !,
        height(X3,3), X3 == a, write('.'), !,
        findall(X4, height(X4, 1), L4), sort(L4, [b,e,i,j,o]), write('.'), !,
        findall(X5, height(X5, 2), L5), sort(L5, [c,h,n]), write('.'), !,
        writeln('Exercițiul 7e rezolvat corect!'),
        !.

%% -----------------------------------------------------------------------------
%% 7f. (bonus: 1p)
%% isBalanced/1
%% isBalanced(?Nod)

isBalanced(_):- fail.

check7f:-
        isBalanced(a), isBalanced(h), isBalanced(m), write('.'), !,
        \+ isBalanced(n), write('.'), !,
        findall(X, isBalanced(X), L), sort(L, [a,b,c,d,e,f,g,h,i,j,k,l,m,o,p]),
        write('.'), !,
        writeln('Exercițiul 7f rezolvat corect!'),
        !.


check7:- check7a, check7b, check7c, check7d, check7e, check7f.

%% -----------------------------------------------------------------------------
%% 8. (bonus: 3p)
%% mergeSort/2
%% mergeSort(+List,-SortedList)
%% 'SortedList' conține elementele listei 'List' în ordine crescătoare
%% Satisfacerea predicatul mergeSort trebuie să realizeze sortarea listei
%% 'List' prin metoda mergeSort.
%% Se vor scrie 2 predicate auxiliare: split/3 și merge/3.

mergeSort(_,_).

check8:-
        mergeSort([], []), write('.'),
        mergeSort([1], [1]), write('.'),
        mergeSort([1,3,2], [1,2,3]), write('.'),
        mergeSort([7,4,1,3,4,2], [1,2,3,4,4,7]), write('.'),
        mergeSort([7,4,1,3,4,2], L4), L4 == [1,2,3,4,4,7], write('.'),
        writeln('Exercițiul BONUS rezolvat corect!'),
        !.

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------

:-dynamic(punct/2).

check:-
        retractall(punct(_, _)),
        once((check1, assert(punct(1, 1)) ; assert(punct(1, 0)))),
        once((check2a, assert(punct(21, 0.5)) ; assert(punct(21, 0)))),
        once((check2b, assert(punct(22, 0.5)) ; assert(punct(22, 0)))),
        once((check3, assert(punct(1, 1)) ; assert(punct(3, 0)))),
        once((check4a, assert(punct(41, 0.5)) ; assert(punct(41, 0)))),
        once((check4b, assert(punct(42, 0.5)) ; assert(punct(42, 0)))),
        once((check5, assert(punct(5, 1)) ; assert(punct(5, 0)))),
        once((check6, assert(punct(6, 1)) ; assert(punct(6, 0)))),
        once((check7a, assert(punct(71, 0.5)) ; assert(punct(71, 0)))),
        once((check7b, assert(punct(72, 0.5)) ; assert(punct(72, 0)))),
        once((check7c, assert(punct(73, 1)) ; assert(punct(73, 0)))),
        once((check7d, assert(punct(74, 2)) ; assert(punct(74, 0)))),
        once((check7e, assert(punct(75, 1)) ; assert(punct(75, 0)))),
        once((check7f, assert(punct(76, 1)) ; assert(punct(76, 0)))),
        once((check8, assert(punct(8, 3)) ; assert(punct(8, 0)))),
        fail.

check:-
        findall(P, punct(_, P), L),
        sum_list(L, S),
        format('Punctaj total: ~f~n',[S]).
