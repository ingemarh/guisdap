% Script for starting GUISDAP on Windows/Jupyter
%
% Usage:
% Browse to this file from the file explorer
% or from a running Matlab GUI.
%
% *Righ-click* on the icon of this file.
% Select Run.
% Happy Gupping!
%
% mypath='C:\Users\Maria\Documents\guisdap9';
function guisdap(mypath)
    disp(mypath)
    if ~exist(mypath,'dir')
        mypath=fileparts(mfilename('fullpath'));
    end
    if isempty(which('exiftool.exe'))
        winpath=fullfile(mypath,'win');
        winall=genpath(winpath);
        addpath(winall);
    end
    if ~exist('start_GUP','file')
        addpath(fullfile(mypath,'anal'));
        cd(userpath)
    end
    if exist(fullfile(userpath,'gup','mygup'),'dir')
     addpath(fullfile(userpath,'gup','mygup'))
    end
    try
        start_GUP
    catch
    fprintf('Please edit file or add the path to guisdap9/anal directory like\n%s%sanal\n',mypath,filesep)
    end
    clear mypath
end 
% analyse
% ---
