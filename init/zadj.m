% zadj.m: Utility to adjust matrices of waveforms for plotting
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Function to separate columns from a waveform
% matrix so that they do not overlap when plotted. 
% Useful in plotting many of GUP waveforms.
% function res=zadj(curv);
function res=zadj(curv);
maxs=max(curv); mins=min(curv);
[m,n]=size(curv);
res=curv-ones(m,1)*mins+...
    ones(m,1)*[0 cumsum( maxs(1:(n-1))-mins(1:(n-1)) )]*1.1;
