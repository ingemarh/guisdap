% EISCAThdf5_generate_year.m

function EISCAThdf5_generate_year(yearpath,hdf5datapath,List_2BHandled,List_crashed)

%yearpath = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/2015/';
% if strcmp(yearpath(end),'/')
%     yearpath = yearpath(1:end-1);
% end
%pathparts = strsplit(yearpath,'/');
%year = pathparts{end};
if nargin < 4
    List_crashed = {}; end
if nargin < 3
    List_2BHandled = {}; end
if nargin < 2
    hdf5datapath = ''; end
if isempty(hdf5datapath)
    hdf5datapath = fullfile(yearpath,'EISCAT_HDF5'); end

if exist(fullfile(yearpath,'Lists.mat'))
    load(fullfile(yearpath,'Lists.mat'))
else
    List_crashed = {};
    %[~,year] = fileparts(yearpath);
    sitelist = dir(yearpath);
    sitelist = sitelist(~ismember({sitelist.name},{'.','..','EISCAT_HDF5'}));   % ignore '.', '..' and 'EISCAT_HDF5' if exists
    mm = 1;
    for ii = 1:length(sitelist)
        %sitename = sitelist(ii).name;
        exprlist = dir(fullfile(sitelist(ii).folder,sitelist(ii).name));
        exprlist = exprlist(~ismember({exprlist.name},{'.','..'}));   % ignore '.' and '..'
        for jj = 1:length(exprlist)
            %display(['year: ' year ', site: ' sitename ', date: ' exprlist(jj).name])
            List_2BHandled{mm} = fullfile(exprlist(jj).folder,exprlist(jj).name);
            mm = mm + 1;
        end
    end
end

while ~isempty(List_2BHandled)
    try 
        EISCAT_hdf5(List_2BHandled{1},hdf5datapath)
    catch
        len = length(List_crashed);
        List_crashed(len+1) = List_2BHandled(1);
    end
    List_2BHandled(1) = [];
end

save([yearpath 'Lists.mat'],'List_2BHandled','List_crashed')