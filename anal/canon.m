% canon.m: A function that repairs file path names for particular system 
% GUISDAP v.1.81 03-02-27 Copyright EISCAT, Huuskonen&Lehtinen
%
function fn=canon(fn,output);

global t_file
if nargin==1, output=1; end
ext='.mat';
if isempty(t_file)
 t_file=[tempname ext];
end
if isempty(strfind(fn,ext))
  fn=[fn ext];
end
if ~exist(fn,'file')
  if exist([fn '.bz2'])
    fn=[fn '.bz2'];
  elseif exist([fn '.gz'])
    fn=[fn '.gz'];
  end
end
if output, disp(['canon: ' fn]), end
if strfind(fn,'.bz2') & isunix
  unix(['bunzip2 -c ' fn ' >' t_file]); fn=t_file;
elseif strfind(fn,'.gz') & isunix
  unix(['gunzip -c ' fn ' >' t_file]); fn=t_file;
end
