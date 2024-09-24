% canon.m: A function that repairs file names 
% GUISDAP v.8.4 04-08-27 Copyright EISCAT, Huuskonen&Lehtinen
%
function fn=canon(fn,output);
global local
if nargin==1    
  output=1;   
end
ext='.mat';
if isempty(strfind(fn,ext))
  fn=[fn ext];
end
if isempty(strfind(fn,'.bz2')) & isempty(strfind(fn,'.gz')) & ~exist(fn,'file')
 if exist([fn '.bz2'],'file')
  fn=[fn '.bz2'];
 elseif exist([fn '.gz'],'file')
  fn=[fn '.gz'];
 end
end
if output
 disp(['canon: ' fn])
end
ofn=[local.tfile ext];ifn=fn;
if ispc
 ofn=['"' ofn '"']; ifn=['"' ifn '"'];
end
if strfind(fn,'.bz2')
 if strcmp(local.fn.bunzip,'7z')
  gupsystem([local.fn.bunzip ' e -so ',ifn,' > ',ofn]);
 else
  gupsystem([local.fn.bunzip ' -ck ' ifn ' >' ofn]);
 end
 fn=[local.tfile ext];
elseif strfind(fn,'.gz')
 d=cell2mat(gunzip(fn,local.tfile));
 fn=[local.tfile ext];
 movefile(d,fn)
end
