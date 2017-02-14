/* Aggregate library */
/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

maplist(Goal, List1, List2, List3) :-
	maplist_(List1, List2, List3, Goal).

maplist_([], [], [], _):- !.
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], Goal) :-
        call(Goal, Elem1, Elem2, Elem3),
        maplist_(Tail1, Tail2, Tail3, Goal).

maplist(Goal, List1, List2, List3, List4) :-
	maplist_(List1, List2, List3, List4, Goal).

maplist_([], [], [], [], _):- !.
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], Goal) :-
	call(Goal, Elem1, Elem2, Elem3, Elem4),
        maplist_(Tail1, Tail2, Tail3, Tail4, Goal).

		 /*******************************
		 *	     AGGREGATE		*
                 *******************************/

:-meta_predicate(aggregate(+, ^, -)).

%%	aggregate(+Template, :Goal, -Result) is nondet.
%
%	Aggregate bindings in Goal according to Template.  The aggregate/3
%	version performs bagof/3 on Goal.

aggregate(Template, Goal0, Result) :-
        template_to_pattern(bag, Template, Pattern, Goal0, Goal, Aggregate),
        bagof(Pattern, Goal, List),
	aggregate_list(Aggregate, List, Result).

%%	aggregate(+Template, +Discriminator, :Goal, -Result) is nondet.
%
%	Aggregate bindings in Goal according to Template.  The aggregate/4
%	version performs setof/3 on Goal.

aggregate(Template, Discriminator, Goal0, Result) :-
	template_to_pattern(bag, Template, Pattern, Goal0, Goal, Aggregate),
	setof(Discriminator-Pattern, Goal, Pairs),
	pairs_values(Pairs, List),
	aggregate_list(Aggregate, List, Result).

:-meta_predicate(aggregate_all(+, 0, -)).

%%	aggregate_all(+Template, :Goal, -Result) is semidet.
%
%	Aggregate  bindings  in  Goal   according    to   Template.  The
%	aggregate_all/3 version performs findall/3 on   Goal.  Note that
%	this predicate fails if Template contains one or more of min(X),
%	max(X),  min(X,Witness)  or  max(X,Witness)  and   Goal  has  no
%	solutions, i.e., the minumum and  maximum   of  an  empty set is
%	undefined.

aggregate_all(Var, _, _) :-
	var(Var), !,
	instantiation_error(Var).
aggregate_all(count, Goal, Count) :- !,
	aggregate_all(sum(1), Goal, Count).
aggregate_all(sum(X), Goal, Sum) :- !,
	State = state(0),
	(  call(Goal),
	   arg(1, State, S0),
	   S is S0 + X,
	   nb_setarg(1, State, S),
	   fail
	;  arg(1, State, Sum)
	).
aggregate_all(max(X), Goal, Max) :- !,
	State = state(X),
	(  call(Goal),
	   arg(1, State, M0),
	   M is max(M0,X),
	   nb_setarg(1, State, M),
	   fail
	;  arg(1, State, Max),
	   nonvar(Max)
	).
aggregate_all(min(X), Goal, Min) :- !,
	State = state(X),
	(  call(Goal),
	   arg(1, State, M0),
	   M is min(M0,X),
	   nb_setarg(1, State, M),
	   fail
	;  arg(1, State, Min),
	   nonvar(Min)
	).
aggregate_all(max(X,W), Goal, max(Max,Witness)) :- !,
	State = state(false, _Max, _Witness),
	(  call(Goal),
	   (   State = state(true, Max0, _)
	   ->  X > Max0,
	       nb_setarg(2, State, X),
	       nb_setarg(3, State, W)
	   ;   number(X)
	   ->  nb_setarg(1, State, true),
	       nb_setarg(2, State, X),
	       nb_setarg(3, State, W)
	   ;   type_error(number, X)
	   ),
	   fail
	;  State = state(true, Max, Witness)
	).
aggregate_all(min(X,W), Goal, min(Min,Witness)) :- !,
	State = state(false, _Min, _Witness),
	(  call(Goal),
	   (   State = state(true, Min0, _)
	   ->  X < Min0,
	       nb_setarg(2, State, X),
	       nb_setarg(3, State, W)
	   ;   number(X)
	   ->  nb_setarg(1, State, true),
	       nb_setarg(2, State, X),
	       nb_setarg(3, State, W)
	   ;   type_error(number, X)
	   ),
	   fail
	;  State = state(true, Min, Witness)
	).
aggregate_all(Template, Goal0, Result) :-
	template_to_pattern(all, Template, Pattern, Goal0, Goal, Aggregate),
        findall(Pattern, Goal, List),
	aggregate_list(Aggregate, List, Result).

%%	aggregate_all(+Template, +Discriminator, :Goal, -Result) is semidet.
%
%	Aggregate  bindings  in  Goal   according    to   Template.  The
%	aggregate_all/4 version performs findall/3 followed by sort/2 on
%	Goal. See aggregate_all/3 to understand   why this predicate can
%	fail.

aggregate_all(Template, Discriminator, Goal0, Result) :-
	template_to_pattern(all, Template, Pattern, Goal0, Goal, Aggregate),
	findall(Discriminator-Pattern, Goal, Pairs0),
	sort(Pairs0, Pairs),
	pairs_values(Pairs, List),
	aggregate_list(Aggregate, List, Result).

template_to_pattern(All, Template, Pattern, Goal0, Goal, Aggregate) :-
	template_to_pattern(Template, Pattern, Post, Vars, Aggregate),
	existential_vars(Goal0, Goal1, AllVars, Vars),
	clean_body((Goal1, Post), Goal2),
	(   All == bag
	->  add_existential_vars(AllVars, Goal2, Goal)
	;   Goal = Goal2
	).

existential_vars(Var, Var, T, T):-
        var(Var), !.
existential_vars(Var^G0, G, T, T1):- !,
        T = [Var|T2],
        existential_vars(G0, G, T2, T1).
existential_vars(M:G0, M:G, T, T1):- !,
        existential_vars(G0, G, T, T1).
existential_vars(G, G, T, T).

add_existential_vars([], G, G).
add_existential_vars([H|T], G0, H^G1) :-
	add_existential_vars(T, G0, G1).


%%	clean_body(+Goal0, -Goal) is det.
%
%	Remove redundant =true= from Goal0.

clean_body((Goal0,Goal1), Goal) :- !,
	clean_body(Goal0, GoalA),
	clean_body(Goal1, GoalB),
	(   GoalA == true
	->  Goal = GoalB
	;   GoalB == true
	->  Goal = GoalA
	;   Goal = (GoalA,GoalB)
	).
clean_body(Goal, Goal).


%%	template_to_pattern(+Template, -Pattern, -Post, -Vars, -Aggregate)
%
%	Determine which parts of the goal we must remember in the
%	findall/3 pattern.
%
%	@param Post is a body-term that evaluates expressions to reduce
%		    storage requirements.
%	@param Vars is a list of intermediate variables that must be
%		    added to the existential variables for bagof/3.
%	@param Aggregate defines the aggregation operation to execute.

template_to_pattern(Term, Pattern, Goal, Vars, Aggregate) :-
	templ_to_pattern(Term, Pattern, Goal, Vars, Aggregate), !.
template_to_pattern(Term, Pattern, Goal, Vars, term(MinNeeded, Functor, AggregateArgs)) :-
	compound(Term), !,
	Term =.. [Functor|Args0],
	templates_to_patterns(Args0, Args, Goal, Vars, AggregateArgs),
	needs_one(AggregateArgs, MinNeeded),
	Pattern =.. [Functor|Args].
template_to_pattern(Term, _, _, _, _) :-
	invalid_template(Term).

templ_to_pattern(sum(X),	   X,	      true,    [],   sum) :- var(X), !.
templ_to_pattern(sum(X0),	   X,	      X is X0, [X0], sum) :- !.
templ_to_pattern(count,	           1,	      true,    [],   count) :- !.
templ_to_pattern(min(X),	   X,	      true,    [],   min) :- var(X), !.
templ_to_pattern(min(X0),	   X,	      X is X0, [X0], min) :- !.
templ_to_pattern(min(X0, Witness), X-Witness, X is X0, [X0], min_witness) :- !.
templ_to_pattern(max(X0),	   X,	      X is X0, [X0], max) :- !.
templ_to_pattern(max(X0, Witness), X-Witness, X is X0, [X0], max_witness) :- !.
templ_to_pattern(set(X),	   X,	      true,    [],   set) :- !.
templ_to_pattern(bag(X),	   X,	      true,    [],   bag) :- !.

templates_to_patterns([], [], true, [], []).
templates_to_patterns([H0], [H], G, Vars, [A]) :- !,
	sub_template_to_pattern(H0, H, G, Vars, A).
templates_to_patterns([H0|T0], [H|T], (G0,G), Vars, [A0|A]) :-
	sub_template_to_pattern(H0, H, G0, V0, A0),
	append(V0, RV, Vars),
	templates_to_patterns(T0, T, G, RV, A).

sub_template_to_pattern(Term, Pattern, Goal, Vars, Aggregate) :-
	templ_to_pattern(Term, Pattern, Goal, Vars, Aggregate), !.
sub_template_to_pattern(Term, _, _, _, _) :-
	invalid_template(Term).

invalid_template(Term) :-
	callable(Term), !,
	domain_error(aggregate_template, Term).
invalid_template(Term) :-
	type_error(aggregate_template, Term).

%%	needs_one(+Ops, -OneOrZero)
%
%	If one of the operations in Ops needs at least one answer,
%	unify OneOrZero to 1.  Else 0.

needs_one(Ops, 1) :-
	member(Op, Ops),
	needs_one(Op), !.
needs_one(_, 0).

needs_one(min).
needs_one(min_witness).
needs_one(max).
needs_one(max_witness).

%%	aggregate_list(+Op, +List, -Answer) is semidet.
%
%	Aggregate the answer  from  the   list  produced  by  findall/3,
%	bagof/3 or setof/3. The latter  two   cases  deal  with compound
%	answers.
%
%	@tbd	Compile code for incremental state update, which we will use
%		for aggregate_all/3 as well.  We should be using goal_expansion
%		to generate these clauses.

aggregate_list(bag, List0, List) :- !,
	List = List0.
aggregate_list(set, List, Set) :- !,
	sort(List, Set).
aggregate_list(sum, List, Sum) :-
	sum_list(List, Sum).
aggregate_list(count, List, Count) :-
	length(List, Count).
aggregate_list(max, List, Sum) :-
	max_list(List, Sum).
aggregate_list(max_witness, List, max(Max, Witness)) :-
	max_pair(List, Max, Witness).
aggregate_list(min, List, Sum) :-
	min_list(List, Sum).
aggregate_list(min_witness, List, min(Min, Witness)) :-
	min_pair(List, Min, Witness).
aggregate_list(term(0, Functor, Ops), List, Result) :- !,
        maplist(state0, Ops, StateArgs, FinishArgs),
        State0 =.. [Functor|StateArgs],
        aggregate_term_list(List, Ops, State0, Result0),
	finish_result(Ops, FinishArgs, Result0, Result).
aggregate_list(term(1, Functor, Ops), [H|List], Result) :-
	H =.. [Functor|Args],
	maplist(state1, Ops, Args, StateArgs, FinishArgs),
	State0 =.. [Functor|StateArgs],
        aggregate_term_list(List, Ops, State0, Result0),
	finish_result(Ops, FinishArgs, Result0, Result).

aggregate_term_list([], _, State, State):- !.
aggregate_term_list([H|T], Ops, State0, State) :-
        step_term(Ops, H, State0, State1),
	aggregate_term_list(T, Ops, State1, State).


%%	min_pair(+Pairs, -Key, -Value) is det.
%%	max_pair(+Pairs, -Key, -Value) is det.
%
%	True if Key-Value has the  smallest/largest   key  in  Pairs. If
%	multiple pairs share the smallest/largest key, the first pair is
%	returned.

min_pair([M0-W0|T], M, W) :-
	min_pair(T, M0, W0, M, W).

min_pair([], M, W, M, W).
min_pair([M0-W0|T], M1, W1, M, W) :-
	(   M0 < M1
	->  min_pair(T, M0, W0, M, W)
	;   min_pair(T, M1, W1, M, W)
	).

max_pair([M0-W0|T], M, W) :-
	max_pair(T, M0, W0, M, W).

max_pair([], M, W, M, W).
max_pair([M0-W0|T], M1, W1, M, W) :-
	(   M0 > M1
	->  max_pair(T, M0, W0, M, W)
	;   max_pair(T, M1, W1, M, W)
	).

%%	step(+AggregateAction, +New, +State0, -State1).

step(bag,   X, [X|L], L).
step(set,   X, [X|L], L).
step(count, _, X0, X1) :-
	succ(X0, X1).
step(sum,   X, X0, X1) :-
	X1 is X0+X.
step(max,   X, X0, X1) :-
	X1 is max(X0, X).
step(min,   X, X0, X1) :-
	X1 is min(X0, X).
step(max_witness, X-W, X0-W0, X1-W1) :-
	(   X > X0
	->  X1 = X, W1 = W
	;   X1 = X0, W1 = W0
	).
step(min_witness, X-W, X0-W0, X1-W1) :-
	(   X < X0
	->  X1 = X, W1 = W
	;   X1 = X0, W1 = W0
	).
step(term(Ops), Row, Row0, Row1) :-
	step_term(Ops, Row, Row0, Row1).

step_term(Ops, Row, Row0, Row1) :-
	functor(Row, Name, Arity),
        functor(Row1, Name, Arity),
	step_list(Ops, 1, Row, Row0, Row1).

step_list([], _, _, _, _):- !.
step_list([Op|OpT], Arg, Row, Row0, Row1) :-
	arg(Arg, Row, X),
	arg(Arg, Row0, X0),
	arg(Arg, Row1, X1),
        step(Op, X, X0, X1),
        !,
        succ(Arg, Arg1),
        step_list(OpT, Arg1, Row, Row0, Row1).

finish_result(Ops, Finish, R0, R) :-
	functor(R0, Functor, Arity),
	functor(R, Functor, Arity),
	finish_result(Ops, Finish, 1, R0, R).

finish_result([], _, _, _, _):- !.
finish_result([Op|OpT], [F|FT], I, R0, R) :-
	arg(I, R0, A0),
	arg(I, R, A),
	finish_result1(Op, F, A0, A),
	succ(I, I2),
	finish_result(OpT, FT, I2, R0, R).

finish_result1(bag, Bag0, [], Bag) :- !,
	Bag = Bag0.
finish_result1(set, Bag,  [], Set) :- !,
	sort(Bag, Set).
finish_result1(max_witness, _, M-W, R) :- !,
	R = max(M,W).
finish_result1(min_witness, _, M-W, R) :- !,
	R = min(M,W).
finish_result1(_, _, A, A).

%%	state0(+Op, -State, -Finish)

state0(bag,   L, L):- !.
state0(set,   L, L):- !.
state0(count, 0, _):- !.
state0(sum,   0, _):- !.

%%	state1(+Op, +First, -State, -Finish)

state1(bag, X, L, [X|L]) :- !.
state1(set, X, L, [X|L]) :- !.
state1(_,   X, X, _).


sum_list(A, B) :-
        sum_list(A, 0, B).

sum_list([], Sum, Sum).
sum_list([Value|Tail], Sum, Result) :-
        NewSum is Sum+Value,
        sum_list(Tail, NewSum, Result).

max_list([First|List], Max) :-
        max_list(List, First, Max).

max_list([], Max, Max).
max_list([Value|Tail], Max, Result) :-
        NewMax is max(Max,Value),
        max_list(Tail, NewMax, Result).

min_list([First|List], Min) :-
        min_list(List, First, Min).

min_list([], Min, Min).
min_list([Value|Tail], Min, Result) :-
        NewMin is min(Min,Value),
        min_list(Tail, NewMin, Result).


:-meta_predicate(call(0,?,?,?)).

call(A,B,C,D):-
        ( A = Module:InGoal->
            InGoal =.. [Name|Args],
            once(append(Args, [B,C,D], NewArgs)),
            Goal =.. [Name|NewArgs],
            call(Module:Goal)
        ; A =.. [Name|Args],
          append(Args, [B,C,D], NewArgs),
          Goal =.. [Name|NewArgs],
          call(Goal)
        ).

:-meta_predicate(call(0,?,?,?,?)).

call(A,B,C,D,E):-
	( A = Module:InGoal->
            InGoal =.. [Name|Args],
            append(Args, [B,C,D,E], NewArgs),
            Goal =.. [Name|NewArgs],
            call(Module:Goal)
        ; A =.. [Name|Args],
          append(Args, [B,C,D,E], NewArgs),
          Goal =.. [Name|NewArgs],
          call(Goal)
        ).

pairs_values([], []).
pairs_values([_-A|B], [A|C]) :-
        pairs_values(B, C).

