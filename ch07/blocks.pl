#!/usr/local/bin/pl -q -t main -f
%%
%   -*- Mode: Prolog -*-
%   NAME:               blocks.pl
%
%   STARTED:            Sun Jun  9 18:32:52 2002
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

%
%    Database by brick:
%    
% shape(b1, brick).
% color(b1, green).
% size(b1, small).
% supported-by(b1, b2).
% supported-by(b1, b3).
% shape(b2, brick).
% color(b2, red).
% size(b2, small).
% supports(b2, b1).
% left-of(b2, b3).
% shape(b3, brick).
% color(b3, red).
% size(b3, small).
% supports(b3, b1).
% right-of(b3, b2).
% shape(b4, pyramid).
% color(b4, blue).
% size(b4, large).
% supported-by(b4, b5).
% shape(b5, cube).
% color(b5, green).
% size(b5, large).
% supports(b5, b4).
% shape(b6, brick).
% color(b6, purple).
% size(b6, large).

%
%    Database by predicate:
%    
color(b1, green).
color(b2, red).
color(b3, red).
color(b4, blue).
color(b5, green).
color(b6, purple).

left_of(b2, b3).
right_of(b3, b2).

shape(b1, brick).
shape(b2, brick).
shape(b3, brick).
shape(b4, pyramid).
shape(b5, cube).
shape(b6, brick).

size(b1, small).
size(b2, small).
size(b3, small).
size(b4, large).
size(b5, large).
size(b6, large).

supported_by(B1, B2) :-
	supports(B2, B1).
% supported_by(b1, b2).
% supported_by(b1, b3).
% supported_by(b4, b5).

supports(b2, b1).
supports(b3, b1).
supports(b5, b4).

supported_by_cube(B) :-
	supported_by(B, B1),
	shape(B1, cube).
