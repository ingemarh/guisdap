% canon.m: A function that repairs file path names for any particular system 
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%
function res=canon(fn,output);

if nargin==1, output=1; end
res=fn;
if strfind(res,'?')
  res=ls([res '.mat']); res(find(res<32))=[];
end
if output, disp(['canon: ' res]), end
