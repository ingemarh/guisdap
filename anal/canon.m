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
if strfind(fn,'.bz2')
 if isunix
 % should work on posix-compliant systems: linux, OSX, and even Cygwin
  prog='lbunzip2'; %faster
  if unix(['which ' prog ' >/dev/null 2>&1']), prog='bunzip2'; end
  gupsystem([prog ' -ck ' fn ' >' local.tfile ext]);
 elseif ispc
  winbzip=which('bzcat.exe');
  if ~isempty(winbzip)
   TS=['"',winbzip,'" -k "',fn,'" > "',local.tfile,ext,'"'];
   system(TS);
  else
   if isfile("C:\Program Files\7-zip\7z.exe")
    system(['"C:\Program Files\7-Zip\7z.exe" e -so "',fn,'" > "',local.tfile,ext,'"']);
   else
    error('Make sure that 7-Zip is installed in the default location (C:\Program Files\7-zip\)')
   end
  end
 end
 fn=[local.tfile ext];
elseif strfind(fn,'.gz')
 if isunix
  gupsystem(['gunzip -c ' fn ' >' local.tfile ext]);
 elseif ispc
  wingzip=which('gzip.exe');
  if ~isempty(wingzip)
   TS=['"',wingzip,'" -k "',fn,'" > "',local.tfile,ext,'"']; %%??? this needs to be tested...
   system(TS);
  else
   if isfile("C:\Program Files\7-zip\7z.exe")
    system(['"C:\Program Files\7-zip\7z.exe" e -so "',fn,'" > "',local.tfile,ext,'"']);
   else
    error('Make sure that 7-Zip is installed in the default location (C:\Program Files\7-zip\)')
   end
  end
 end
 fn=[local.tfile ext];
end
