% integrate.m: interface to Nigel Wade's integration package
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Integrate is a mex-call to Nigel Wade's integration package
% This m-file is provided to point out the mex-routine integrate is not present
% or that the search path is not properly set up
%
function [a,s,d,f,g,h,j,k,l]=integrate(a)
fprintf(' integrate.mex was not found at the search path\n')
fprintf(' it is needed to read the EISCAT raw data files\n')
error(' Stopping execution')
