% t_stop: timing routine xxx
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: t_init
function t_stop(i),

global fl_stop ti_stop

%fl_stop(i)=fl_stop(i)+flops;
ti_stop(i)=ti_stop(i)+cputime;  %AH 94-6-14 
