% t_start.m: timing routine xxx
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% See also: t_init
% t_start.m
function t_start(i),

global ti_start fl_start ti_calls

ti_calls(i)=ti_calls(i)+1;
ti_start(i)=ti_start(i)+cputime; %AH 94-6-14 
%fl_start(i)=fl_start(i)+flops;
