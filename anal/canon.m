% canon.m: A function that repairs file path names for any particular system 
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%
function res=canon(fn,output);

global t_file
if nargin==1, output=1; end
res=fn; ext='.mat';
if isempty(t_file)
 t_file=[tempname ext];
end
if strfind(fn,'?')
  fn=[fn ext]; j=0;
  while ~exist(res) & j<9
    res=ls(fn); res(find(res<32))=[]; j=j+1;
  end
elseif strfind(fn,'*')
  [dirs,fn]=fileparts(res);
  dp=fileparts(dirs);
  dirs=dir(dirs);
  for i=1:length(dirs)
    res=fullfile(dp,dirs(i).name,[fn ext]);
    if ~exist(res) & exist([res '.bz2']) & isunix
     unix(['bunzip2 -c ' res '.bz2 >' t_file]); res=t_file;
    end
    if ~isempty(dir(res)), break, end
  end
end
if output, disp(['canon: ' res]), end
