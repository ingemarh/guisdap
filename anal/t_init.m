% t_init.m: Initialization for a set of timing routines.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% Not very user friendly or useful, but used in the analyis package
% The calls must be in the following order
% t_init     % Initialization
% t_start(1) % start of first timing block
% t_start(2) % start of second timing block within the first one
% t_start(3) % start of third timing block within the second one
% t_stop(3) % end of block 3
% t_stop(2) % end of block 2
% t_stop(1) % end of block 1
% t_result  % output of time used
% function t_init
  function t_init
		
global ti_start ti_stop fl_start fl_stop ti_calls

ti_start=zeros(1,10);
ti_stop=zeros(1,10);
fl_start=zeros(1,10);
fl_stop=zeros(1,10);
ti_calls=zeros(1,10);
