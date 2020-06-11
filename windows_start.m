% Script for starting GUISDAP on Windows.
%
% Usage:
% Browse to this file from the file explorer
% or from a running Matlab GUI.
%
% *Righ-click* on the icon of this file.
% Select Run.
% Happy Gupping!
%
% mypath='C:\Users\myuser\mydir\guisdap9';
try
 mypath=pwd;
 addpath(fullfile(mypath,'anal'))
 start_GUP
 cd(userpath)
catch
 disp(sprintf(['Please edit %s, or add the path to guisdap9/anal directory'],mfilename('fullpath')))
end
% analyse
% ---
