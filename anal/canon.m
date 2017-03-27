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
        system(['bunzip2 -c ' fn ' >' local.tfile ext]);
   elseif ispc
         %We run on Windows. adapt this path as needed
        system(['"C:\Program Files\7-Zip\7z.exe" e -so "' fn '" >' local.tfile ext] ); 
   end
	fn=[local.tfile ext];
    
elseif strfind(fn,'.gz')
    if isunix
        system(['gunzip -c ' fn ' >' local.tfile ext]); 
    elseif ispc
        system(['"c:\Program Files\7-zip\7z.exe" e -so "' fn '" >' local.tfile ext] ); 
    end
	fn=[local.tfile ext];
end
