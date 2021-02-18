function EISCAThdf5_generate_year(yearpath,datapath,matfile_lists)
% EISCAThdf5_generate_year(yearpath,datapath,matfile_lists)
%
% Generate EISCAT HDF5-files from one year of Madrigal data.
%
% yearpath:         path to folder specifying which year to handle (E.g in eiscathq: '/opt/madrigal/experiments/yyyy')
% datapath:         path to stored EISCAT HDF5-files, where a folder datapath/EISCAT/yyyy is created 
%                   including all files and lists. If datapath is left out it is set to current folder (pwd).
% matfile_lists:    .mat-file containing lists of dates 
%                       1) to be handled (List_2BHandled)
%                       2) that were handled and were OK (List_HandledOK) 
%                       3) that crashed (List_Crashed)
%                   If 'matfile_lists' is left out these list are set and the whole year (specified by 'yearpath') is handled

if nargin < 3
    matfile_lists = ''; end
if nargin < 2
    datapath = ''; end
if nargin < 1
    error('Specify yearly data to be handled.'); end

if strcmp(yearpath(end),'/')
    yearpath = yearpath(1:end-1);
end
pathparts = strsplit(yearpath,'/');
year = pathparts{end};

if isempty(datapath)
    datapath = fullfile(pwd,'EISCAT',year);
else
    datapath = fullfile(datapath,'EISCAT',year);
end

% check if the folder 'datapath' is not empty (contains folders or files) and make error if matfile_lists is empty, in order to not overwrite already handled data.
datapath_content = dir(datapath);
datapath_content = datapath_content(~ismember({datapath_content.name},{'.','..'}))  % ignoring '.' and '..'
if isempty(matfile_lists) && ~isempty(datapath_content)
    error(['There is content in ' datapath '. The routine is stopped in order to not delete/overwrite this content. Run command again with the "matfile_list" (found in ' datapath ') as input or empty/delete ' datapath '.'])
end

sitelist = dir(yearpath);
sitelist = sitelist(~ismember({sitelist.name},{'.','..','ssr','trd','srd','lrd'}));   % ignoring '.', '..', 'ssr', 'srd', 'trd' 
mm = 1;
for ii = 1:length(sitelist)
    exprlist = dir(fullfile(sitelist(ii).folder,sitelist(ii).name));
    exprlist = exprlist(~ismember({exprlist.name},{'.','..'}) & ~endsWith({exprlist.name},{'_vhf','_uhf','_32m','_42m','_sod'}));   % ignore '.' and '..'
    for jj = 1:length(exprlist)
        List_all{mm} = fullfile(exprlist(jj).folder,exprlist(jj).name);
        mm = mm + 1;
    end
end

if isempty(matfile_lists)
    List_2BHandled = List_all;
    List_Crashed = {};
    List_HandledOK = {};
else
    load(matfile_lists)    % Should include *List_2BHandled, *List_HandledOK, and *List_Crashed
   
    %check if new analysed experiments have been added 
    List_allold = [List_2BHandled List_HandledOK List_Crashed'];
    List_check = [List_allold List_all];
    [Unika,~,X] = unique(List_check);
    Y = hist(X,unique(X));
    List_newadd = Unika(find(Y == 1));
    if ~isempty(List_newadd)
    	List_2BHandled = [List_2BHandled List_newadd];
        display(['New experiments have been added to ' year ' and are handled.'])
    end
end


if isempty(List_2BHandled) % check if List_2BHandled is empty
    if ~isempty(List_Crashed)
        List_2BHandled = List_Crashed;
        List_Crashed = {};
        display('Handling files in List_Crashed')
    else
        display('Nothing to handle, both List_Crashed and List_2BHandled are empty')
    end
end


while ~isempty(List_2BHandled)
    try 
        EISCAT_hdf5(List_2BHandled{1},datapath)
        lenok = length(List_HandledOK);
        List_HandledOK(lenok+1) = List_2BHandled(1);
    catch
        lencr = length(List_Crashed);
        %display(['Crashed somewhere in ' List_2BHandled{1}])
        display([lasterr newline])
        List_Crashed(lencr+1) = List_2BHandled(1);
    end
    List_2BHandled(1) = [];
    delete([datapath '/Lists*.mat'])
    save([datapath '/Lists_' datestr(now,'yyyy-mm-dd_HHMM') '.mat'],'List_2BHandled','List_HandledOK','List_Crashed')
end
