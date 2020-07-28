#!/usr/local/bin/pl -q -t main -f
%%
%   -*- Mode: Prolog -*-
%   NAME:               genealogy.pl
%
%   STARTED:            Sat Jul  6 00:00:20 2002
%   MODIFICATIONS:
%
%   PURPOSE:
%
%
%
%   CALLING SEQUENCE:
%
%
%   INPUTS:
%
%   OUTPUTS:
%
%   EXAMPLE:
%
%   NOTES:
%
%%

parent(colin, suzanne).
parent(deirdre, suzanne).
parent(arthur, bruce).
parent(kate, bruce).
parent(arthur, charles).
parent(kate, charles).
parent(arthur, david).
parent(kate, david).
parent(arthur, ellen).
parent(kate, ellen).
parent(frank, george).
parent(linda, george).
parent(frank, hillary).
parent(linda, hillary).
parent(bruce, tamara).
parent(suzanne, tamara).
parent(bruce, vincent).
parent(suzanne, vincent).
parent(george, ivan).
parent(ellen, ivan).
parent(george, julie).
parent(ellen, julie).
parent(george, marie).
parent(ellen, marie).
parent(andre, nigel).
parent(hillary, nigel).
parent(tamara, frederick).
parent(vincent, zelda).
parent(wanda, zelda).
parent(ivan, joshua).
parent(wanda, joshua).
parent(quentin, robert).
parent(julie, robert).
parent(nigel, olivia).
parent(marie, olivia).
parent(nigel, peter).
parent(marie, peter).
parent(robert, yvette).
parent(zelda, yvette).
parent(peter, diane).
parent(erica, diane).

male(colin).
male(arthur).
male(frank).
male(andre).
male(quentin).
male(bruce).
male(charles).
male(david).
male(george).
male(vincent).
male(ivan).
male(nigel).
male(frederick).
male(joshua).
male(robert).
male(peter).

female(deirdre).
female(kate).
female(linda).
female(wanda).
female(erica).
female(suzanne).
female(ellen).
female(hillary).
female(tamara).
female(julie).
female(marie).
female(zelda).
female(olivia).
female(yvette).
female(diane).

father(X, Person) :-
	male(X),
	parent(X, Person).

mother(X, Person) :-
	female(X),
	parent(X, Person).

% parents([F, M], Person) :-
% 	father(F, Person),
% 	mother(M, Person).
% parents([F], Person) :-
% 	father(F, Person).
% parents([M], Person) :-
% 	mother(M, Person).
% parents([], _).

parents(P, Person) :-
	findall(Parent, parent(Parent, Person), P).

% children([C | T], Person) :-
% 	parent(Person, C),
% 	children(T, Person).
% children([], _).
children(C, Person) :-
	findall(Child, parent(Person, Child), C).

siblings(S, Person) :-
	father(F, Person),
	mother(M, Person),
	children(C1, F),
	children(C2, M),
	union(C1, C2, S1),
	delete(S1, Person, S).
siblings(S, Person) :-
	father(F, Person),
	children(S1, F),
	delete(S1, Person, S).
siblings(S, Person) :-
	mother(M, Person),
	children(S1, M),
	delete(S1, Person, S).
siblings([], _).	

grandparents(G, Person) :-
	father(F, Person),
	mother(M, Person),
	parents(PF, F),
	parents(PM, M),
	union(PF, PM, G).
grandparents(G, Person) :-
	father(F, Person),
	parents(G, F).
grandparents(G, Person) :-
	mother(M, Person),
	parents(G, M).
grandparents([], _).

%cousins(C, Person) :-

descended_from(X, Y) :-
	parent(Y, X).
descended_from(X, Y) :-
	parent(Y, Z),
	descended_from(X, Z).

ancestors(A, Person) :-
	setof(Ancestor, descended_from(Person, Ancestor), A).

generation_gap(A, Person, 1) :-
	parent(A, Person).
generation_gap(A, Person, Gap) :-
	descended_from(Person, A),
	father(F, Person),
	generation_gap(A, F, G),
	Gap is G + 1.
generation_gap(A, Person, Gap) :-
	descended_from(Person, A),
	mother(M, Person),
	generation_gap(A, M, G),
	Gap is G + 1.
	