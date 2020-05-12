% Generate an EISCAT HDF5-file from mat-files generated in a Guisdap analysis

function [storepath,EISCAThdf5file] = matvecvel2hdf5(matfile_vel,datapath)%,addfigs,addnotes) 

global path_GUP %result_path 
% if nargin<4, addnotes = []; else addnotes = 1; end 
% if nargin<3, addfigs = []; else addfigs = 1; end 
% if nargin==1, error('Not enough input parameters, path to matfiles folder and path to datastore folder needed'); end
% if nargin<1
%     matpath = result_path;
%     datapath = result_path;
% end
% if isstring(datapath)
%     datapath = char(datapath);    % need to be char class
% end

load(matfile_vel)

GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file
[~,text] = xlsread(GuisdapParFile);
parameters_list = text(:,1);

vdate = [];
vpos  = [];
vg    = [];
vgv   = [];
nrec  = [];

% Sorting time
t1all=unique(sort(Vdate(1,:)));
for t1=t1all
    d=find(Vdate(1,:)==t1);
    t2all=unique(sort(Vdate(2,d)));
    for t2=t2all
        d1=d(find(Vdate(2,d)==t2));
        nrec = [nrec; length(d1)];
        vdate = [vdate;Vdate(:,d1)'];
        vpos  = [vpos;Vpos(d1,:)]; 
        vg    = [vg;Vg(d1,:)];
        vgv   = [vgv;Vgv(d1,:)];
    end
end

a = find(strcmp('nrec',parameters_list)==1);
[~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};        
if length(unique(nrec)) == 1
    matfile.data.par0d = nrec(1);
    matfile.metadata.par0d = info';
else
    matfile.data.par1d = nrec;
    matfile.metadata.par1d = info';
end
    
if exist('Vdate','var')
    parameters_vdate = {'time1' 'time2'};
    matfile.data.utime = [posixtime(datetime(datestr(vdate(:,1)))) posixtime(datetime(datestr(vdate(:,1))))];
    for ii = 1:2
        a = find(strcmp(char(parameters_vdate(ii)),parameters_list)==1);
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(4) = {'Vdate'};
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.utime(:,ii) = info';
    end
end

n = 0;
if exist('Vpos','var')
    parameters_vpos = {'h_h' 'lon' 'lat'};
    matfile.data.par2d(:,n+1:n+3) = vpos;
    for ii = 1:3
        n = n + 1; 
        a = find(strcmp(char(parameters_vpos(ii)),parameters_list)==1);
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(4) = {'Vpos'};
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.par2d(:,n) = info';
    end
end
if exist('Vg','var')
    parameters_vg = {'vi_east' 'vi_north' 'vi_up'};
    matfile.data.par2d(:,n+1:n+3) = vg;
    for ii = 1:length(vg(1,:))
        n = n + 1; 
        a = find(strcmp(char(parameters_vg(ii)),parameters_list)==1);
        info(4) = {'Vg'};
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.par2d(:,n) = info';
    end
end
if exist('Vgv','var')
    parameters_vgv = {'dvi_east' 'dvi_north' 'dvi_up'};
    matfile.data.par2d(:,n+1:n+3) = sqrt(vgv(:,1:3));
    for ii = 1:3
        n = n + 1; 
        a = find(strcmp(char(parameters_vgv(ii)),parameters_list)==1);
        info(4) = {'Vgv'};
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.par2d(:,n) = info';
    end
end


% store the Vinput content
if exist('Vinputs','var')
    Vinputs_Fields = fieldnames(Vinputs);
    for jj = 1:length(Vinputs)
        for field = Vinputs_Fields.'
            metapar = [];
            if ischar(Vinputs(jj).(char(field)))
                matfile.metadata.Vinputs.(char(field))(jj) = {Vinputs(jj).(char(field))}; 
            else
                matfile.metadata.Vinputs.(char(field))(jj) = {num2str(Vinputs(jj).(char(field)))};
            end
        end      
    end
end
for field = Vinputs_Fields.'
    matfile.metadata.Vinputs.(char(field)) = matfile.metadata.Vinputs.(char(field))';
end
    
starttime = datestr(t1,'yyyy-mm-ddTHH:MM:SS');
endtime   = datestr(t2,'yyyy-mm-ddTHH:MM:SS');

software = 'https://git.eiscat.se/cvs/guisdap9';
level2_link = '';
matfile.metadata.software_link = {software};
if ~isempty(level2_link)
    matfile.metadata.level2_links = {level2_link};
end

r_time = datevec(Vdate(1,1));
starttime = datestr(r_time);

year = num2str(r_time(1));
month = sprintf('%02d',r_time(2));
day = sprintf('%02d',r_time(3));

hour   = sprintf('%02d',r_time(4));
minute = sprintf('%02d',r_time(5));
second = sprintf('%02.f',r_time(6));

e_time = datevec(Vdate(2,end));
endtime = datestr(e_time);

if ~exist('name_expr','var'), name_expr=''; end
 
[~,filename,~] = fileparts(matfile_vel); 
storepath = fullfile(datapath,['EISCAT_' filename]);
if exist(storepath)
   rmdir(storepath,'s');
end
mkdir(storepath);

Hdf5File = sprintf('%s%s%s','EISCAT_',filename,'.hdf5');
MatFile = sprintf('%s%s%s','EISCAT_',filename,'.mat');
hdffilename = fullfile(storepath,Hdf5File);
matfilename = fullfile(storepath,MatFile);
EISCAThdf5file = hdffilename;
GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file

if exist(hdffilename)==2, delete(hdffilename); end

[~,text] = xlsread(GuisdapParFile);
parameters_list = text(:,1);    % list that includes all Guisdap parameters and keep their positions from the excel arc

matfile.metadata.header= text(1,1:7)';

% TAI time (leapseconds)
[~,leaps1] = utc2tai(datestr(datetime(matfile.data.utime(:,1),'ConvertFrom','posixtime')),'utc2tai');      % leap seconds between utc --> tai format
[~,leaps2] = utc2tai(datestr(datetime(matfile.data.utime(:,2),'ConvertFrom','posixtime')),'utc2tai');
leaps = [leaps1 leaps2];
if length(unique(leaps)) == 1
    ll0 = length(matfile.metadata.par0d);
    matfile.data.par0d(ll0+1) = leaps1(1);  
    a = find(strcmp('leaps',parameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par0d(:,ll0+1) = info';
else
    ll1 = length(matfile.metadata.par1d(1,:));
    matfile.data.par1d(:,ll1+1:ll1+2) = leaps;
    a = find(strcmp('leaps1',parameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par1d(:,ll1+1) = info';
    a = find(strcmp('leaps2',parameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par1d(:,ll1+2) = info';
end

nn = 0;
if exist('name_expr','var'); nn = nn + 1;
    infoname(1) = {'name_expr'};
    infoname(2) = {name_expr};
    a = find(strcmp('name_expr',parameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_ant','var'); nn = nn + 1; 
    infoname(1) = {'name_ant'};
    infoname(2) = {name_ant};
    infoname(3) = {'name of receiving antenna, or alternative code name for multistatic analyses (tro: Tromso, kst: combination of Kiruna, Sodankyla, Tromso, esr: Svalbard, esa: combination of mainland and Svalbard'};
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_sig','var'); nn = nn + 1; 
    infoname(1) = {'name_sig'};
    infoname(2) = {name_sig};
    a = find(strcmp('name_sig',parameters_list)==1); 
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';   
end
if exist('name_strategy','var'); nn = nn + 1;
    infoname(1) = {'name_strategy'};
    infoname(2) = {name_strategy};
    infoname(3) = {'limitations applied and used in the velocity-vector calculations'};
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_exps','var'); nn = nn + 1; 
    infoname(1) = {'name_exps'};
    for i = 1:length(name_exps(:,1))
        if i == 1
            nameexps = name_exps(1,:);
        else
            nameexps = [nameexps ', ' name_exps(i,:)];
        end
    end
    infoname(2) = {nameexps};
    a = find(strcmp('name_expr',parameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_ants','var'); nn = nn + 1; 
    infoname(1) = {'name_ants'};
    for i = 1:length(name_ants(:,1))
        if i == 1
            nameants = name_ants(1,:);
        else
            nameants = [nameants ', ' name_ants(i,:)];
        end
    end
    infoname(2) = {nameants};
    a = find(strcmp('name_ant',parameters_list)==1); 
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_strategies','var'); nn = nn + 1;
    infoname(1) = {'name_strategies'};
    infoname(2) = {name_strategies};
    a = find(strcmp('name_strategy',parameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('GUP_ver','var'); nn = nn + 1; 
    infoname(1) = {'gupver'};
    infoname(2) = {num2str(GUP_ver)};
    a = find(strcmp('h_ver',parameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end

if isfield(matfile.metadata,'par0d')
    aa = find(cellfun('isempty',matfile.metadata.par0d(6,:)));    matfile.metadata.par0d(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par0d(7,:)));    matfile.metadata.par0d(7,aa)= {'0'};
end
if isfield(matfile.metadata,'par1d')
    aa = find(cellfun('isempty',matfile.metadata.par1d(6,:)));    matfile.metadata.par1d(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par1d(7,:)));    matfile.metadata.par1d(7,aa)= {'0'};
end
if isfield(matfile.metadata,'par2d')
    aa = find(cellfun('isempty',matfile.metadata.par2d(6,:)));    matfile.metadata.par2d(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par2d(7,:)));    matfile.metadata.par2d(7,aa)= {'0'};
end
  
symbols = ['a':'z' 'A':'Z' '0':'9'];
strLength = 10;
nums = randi(numel(symbols),[1 strLength]);
randstr = symbols(nums);
PID = ['doi://eiscat.se/3a/' year month day hour minute second '/' randstr];

matfile.metadata.schemes.DataCite.Identifier = {PID};
matfile.metadata.schemes.DataCite.Creator = {name_ant};
matfile.metadata.schemes.DataCite.Title = {['EISCAT_' filename]};
matfile.metadata.schemes.DataCite.Publisher = {'EISCAT Scientific Association'};
matfile.metadata.schemes.DataCite.ResourceType.Dataset = {'Level 3 Ionosphere'};
matfile.metadata.schemes.DataCite.Date.Collected = {[starttime '/' endtime]};

% Find the smallest box (4 corners and mid-point) to enclose the data, 
% or 1 or unique 2 points that describes the data.
% If area of convhull (or distance between 2 points) < 10-4 deg^2, 
% define all points as one (average)
% imag = 1 to plot the data and the corresponding box if there is a box
im = 1;
gg_sp = [Vpos(:,2) Vpos(:,1)];

[plonlat,PointInPol] = polygonpoints([gg_sp(:,2) gg_sp(:,1)],im);
matfile.metadata.schemes.DataCite.GeoLocation.PolygonLon = plonlat(:,1);
matfile.metadata.schemes.DataCite.GeoLocation.PolygonLat = plonlat(:,2);
if ~isempty(PointInPol)
    matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLon = PointInPol(1);
    matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLat = PointInPol(2);
end

% Delete any empty fields from the structure
sFields = fieldnames(matfile);
for sf = sFields.' 
    tFields = fieldnames(matfile.(char(sf)));
    for tf = tFields.'
        if isempty(matfile.(char(sf)).(char(tf)))
            matfile.(char(sf)) = rmfield(matfile.(char(sf)),char(tf));
        end
    end
end

save(matfilename,'matfile')

% Generate an HDF5-file from the MAT-file
chunklim = 10;
sFields = fieldnames(matfile);
for sf = sFields.'
    group1 = ['/' char(sf)];
    tFields = fieldnames(matfile.(char(sf)));
    for tf = tFields.'
        if strcmp('data',char(sf)) && (strcmp('par0d',char(tf)) || strcmp('par1d',char(tf)) || strcmp('par2d',char(tf)) || strcmp('par2d_pp',char(tf)) || strcmp('acf',char(tf)) || strcmp('ace',char(tf)) || strcmp('lag',char(tf)) || strcmp('freq',char(tf)) || strcmp('spec',char(tf)) || strcmp('om',char(tf)))
            npar  = length(matfile.data.(char(tf))(1,:));
            ndata = length(matfile.data.(char(tf))(:,1));
            if ge(ndata,chunklim) && ge(npar,chunklim), csize = [chunklim chunklim];
            elseif ge(ndata,chunklim), csize = [chunklim npar];
            elseif ge(npar,chunklim), csize = [ndata chunklim];
            else csize = [ndata npar]; end    
            h5create(hdffilename,['/' char(sf) '/' char(tf)],size([matfile.(char(sf)).(char(tf))]),'ChunkSize',csize,'Deflate',9,'Datatype','single');
            h5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
        elseif strcmp('metadata',char(sf)) 
            if isstruct(matfile.(char(sf)).(char(tf)))
                group2 = [group1 '/' char(tf)];
                uFields = fieldnames(matfile.(char(sf)).(char(tf)));
                for uf = uFields.'
                    if isstruct(matfile.(char(sf)).(char(tf)).(char(uf)))
                        group3 = [group2 '/' char(uf)];
                        vFields = fieldnames(matfile.(char(sf)).(char(tf)).(char(uf)));
                        for vf = vFields.'
                            if isstruct(matfile.(char(sf)).(char(tf)).(char(uf)).(char(vf)))
                                group4 = [group3 '/' char(vf)];
                                wFields = fieldnames(matfile.(char(sf)).(char(tf)).(char(uf)).(char(vf)));
                                for wf = wFields.'
                                    strdata = matfile.(char(sf)).(char(tf)).(char(uf)).(char(vf)).(char(wf));
                                    dsname = char(wf);
                                    strds2hdf5(hdffilename,group4,dsname,strdata)
                                end
                            else
                                strdata = matfile.(char(sf)).(char(tf)).(char(uf)).(char(vf));
                                dsname = char(vf);
                                strds2hdf5(hdffilename,group3,dsname,strdata)
                            end
                        end
                    else
                        strdata = matfile.(char(sf)).(char(tf)).(char(uf));
                        dsname = char(uf);
                        strds2hdf5(hdffilename,group2,dsname,strdata)
                    end
                end
            else
                strdata = matfile.(char(sf)).(char(tf));
                dsname = char(tf);
                strds2hdf5(hdffilename,group1,dsname,strdata)
            end
        else
            h5create(hdffilename,['/' char(sf) '/' char(tf)],size([matfile.(char(sf)).(char(tf))]));
            h5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
        end
    end   
end