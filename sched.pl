% Author: Julien Hoachuck
% Part 1

% c_Numbers(N): Numbers of all courses
c_numbers(N) :- course(N,_,_).

% c_pl(N): Number of all programming language courses
c_pl(N) :- course(N, programming_languages,_).

% c_notpl(N): Number of all non-programming language courses
c_notpl(N) :- course(N,_P,_),_P \= programming_languages.

% c_inst60(L): List of those teaching 60
c_inst60(L) :- course(60,_,L).

% c_inst60_sorted(L): Sorted list of those teaching 60
c_inst60_sorted(L) :- c_inst60(L),sort(L).


% c_inst20(L): List of those teaching 20
c_inst20(L) :- course(20,_,L).

% c_inst20_sorted(L): Sorted list of those teaching 20
c_inst20_sorted(L) :- c_inst20(L),sort(L).

% c_inst_sorted(N,L): Sorted list of those teaching N
c_inst_sorted(N,L) :- course(N,_,L),sort(L).

% c_single_inst(N): Numbers of courses with exactly one instructor
c_single_inst(N) :- course(N,_,[_H|_T]),_T=[]. 

% c_multi_inst(N): Numbers of courses with multiple instructor
c_multi_inst(N) :- course(N,_,[_H|_T]),_T\=[]. 

% c_exclusive(I,N): numbers of courses for which I is the only instructor
c_exclusive(I,N) :- course(N,_,[H|T]), T=[], I=H.

% c_12_inst_1or: using OR for exactly two instructors or 1 instructor
c_12_inst_1or(N) :- (course(N,_,[_H|T]), T = []);(course(N,_,[_H|T]), T= [_A]).

% c_12_inst_2wo: using no or but two rules
c_12_inst_2wo(N) :- course(N,_,[_H|T]),T=[].
c_12_inst_2wo(N) :- course(N,_,[_H|T]),T=[_A].

% Part 2

delete_question(188).

sortappend(X,Y,Z) :- append(X,Y,Z), sort(Z).

% Part 3
distribute(X,[],[]).
distribute(X,[Y1|Y2],[[X,Y1]|Z]) :- distribute(X,Y2,Z).

% Part 4	
% myfor(L,U,Result) 
myfor(L,U,Result) :- 
	L =< U,
	L1 is L+1,
	myfor(L1,U,Res1),
	Result = [L | Res1].

myfor(L,U,[]) :- L > U.


crossmyfor(0,_B,[]):- !.

crossmyfor(A,B,C):- 	
	myfor(1,B,Y),
	distribute(A,Y,Z),
	R1 is A-1, !,
	crossmyfor(R1,B,K),
	append(Z,K,C),
	sort(C).





% Part 5

%a
getallmeetings([],[]).

getallmeetings(C,Z) :-
	member(E1,C),
	delete(C,E1,C1),
	member(Name,E1),
	delete(E1,Name,[Res|_]),!,
	getallmeetings(C1,Y),
	append(Res,Y,Z),
	sort(Z).

%b

participants([],[]).
participants(C,Z) :-
	getallmeetings(C,E),
	multisearch(E,C,D),!,
	participants(L2,T),
	append(D,T,Z).
	sort(Z).


multisearch([],B,[]).
multisearch(A,B,C) :-
	select(E1,A,L2),!,
	search(E1,B,O),
	append([E1],[O],F),
	multisearch(L2,B,D),
	append([F],D,C).



search(A,[],[]).
search(A,B,C) :- % produces sorted list of names
	select(L1,B,L2),
	select(Name,L1,[Res|_]),!,
	search(A,L2,T),!,
	(member(A,Res) -> append([Name],T,C); append([],T,C)),  
	sort(C).

%c
osched(MR,MH,C,Z) :- 
	crossmyfor(MR,MH,HR),
	participants(C,D),!,
	osched(HR,D,Z).


osched([],[],[]).
osched(HR,C,Z):- 
	select(A,HR,B),
	select(D,C,E),
	osched(B,E,K),
	append([A],[D],F),
	append([F],K,Z).


	



