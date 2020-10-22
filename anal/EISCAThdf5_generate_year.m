function EISCAThdf5_generate_year(yearpath,datapath,matfile_lists,runcrashed)
% EISCAThdf5_generate_year(yearpath,datapath,matfile_lists)
%
% Generate EISCAT HDF5-files from one year of Madrigal data.
%
% yearpath:         path to folder specifying which year to handle (E.g in eiscathq: '/opt/madrigal/experiments/yyyy')
% datapath:         path to stored EISCAT HDF5-files, where a folder datapath/EISCAT_HDF5_yyyy is created 
%                   including all files and lists. If datapath is left out it is set to current folder (pwd).
% matfile_lists:    .mat-file containing lists of dates 
%                       1) to be handled (List_2BHandled)
%                       2) that were handled and were OK (List_HandledOK) 
%                       3) that crashed (List_Crashed)
% runcrashed	    If it is non-empty the experiments in List_Crashed will be run, if and only if List_2BHandled 
%                   is empty.
if nargin < 4
    runcrashed = []; end	
if nargin < 3
    matfile_lists = ''; end
if nargin < 2
    datapath = ''; end
if nargin < 1
    error('Specify yearly data to be handled.'); end

if strcmp(yearpath(end),'/')
    yearpath = yearpath(1:end-1); end
pathparts = strsplit(yearpath,'/');
year = pathparts{end};

if isempty(datapath)
    datapath = fullfile(pwd,'EISCAT',year);
else
    datapath = fullfile(datapath,'EISCAT',year); end

if ~isempty(matfile_lists)
    load(matfile_lists)    % Should include *List_2BHandled, *List_HandledOK, and *List_Crashed
else
    List_Crashed = {};
    List_HandledOK = {};
    sitelist = dir(yearpath);
    sitelist = sitelist(~ismember({sitelist.name},{'.','..','ssr','trd','srd'}));   % ignoring '.', '..', 'ssr', 'srd', 'trd' 
    mm = 1;
    for ii = 1:length(sitelist)
        %sitename = sitelist(ii).name;
        exprlist = dir(fullfile(sitelist(ii).folder,sitelist(ii).name));
        exprlist = exprlist(~ismember({exprlist.name},{'.','..'}) & ~endsWith({exprlist.name},{'_vhf','_uhf','_32m','_42m'}));   % ignore '.' and '..'
        for jj = 1:length(exprlist)
            %display(['year: ' year ', site: ' sitename ', date: ' exprlist(jj).name])
            List_2BHandled{mm} = fullfile(exprlist(jj).folder,exprlist(jj).name);
            mm = mm + 1;
        end
    end
end

if ~isempty(runcrashed)
    if ~isempty(List_2BHandled) % check that List_2BHandled is empty first
        error('List_2BHandled is not empty. Handle these files before running the files in List_Crashed.')
    end
    List_2BHandled = List_Crashed;
    List_Crashed = {};
end

while ~isempty(List_2BHandled)
    try 
        EISCAT_hdf5(List_2BHandled{1},datapath)
        lenok = length(List_HandledOK);
        List_HandledOK(lenok+1) = List_2BHandled(1);
    catch
        lencr = length(List_Crashed);
        display(['Crashed somewhere in ' List_2BHandled{1}])
        List_Crashed(lencr+1) = List_2BHandled(1);
    end
    List_2BHandled(1) = [];
    delete([datapath '/Lists*.mat'])
    save([datapath '/Lists_' datestr(now,'yyyy-mm-dd_HHMM') '.mat'],'List_2BHandled','List_HandledOK','List_Crashed')
end
