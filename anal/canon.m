% canon.m: A function that repairs file path names for any particular system 
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%
function res=canon(fn,output);

if nargin==1, output=1; end
res=fn;
if strfind(fn,'?')
  fn=[fn '.mat']; j=0;
  while ~exist(res) & j<9
    res=ls(fn); res(find(res<32))=[]; j=j+1;
  end
elseif strfind(fn,'*')
  [dirs,fn]=fileparts(res);
  dp=fileparts(dirs);
  dirs=dir(dirs);
  for i=1:length(dirs)
    res=fullfile(dp,dirs(i).name,[fn '.mat']);
    if ~isempty(dir(res)), break, end
  end
end
if output, disp(['canon: ' res]), end
