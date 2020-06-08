% Generate a new EISCAT HDF5 file from an old HDF5-file fromated in Madrigal, called by EISCAT_hdf5.m

function [storepath,EISCAThdf5file] = cedar2hdf5(hdf5file,datapath)

global path_GUP 
hdf5ver = '0.9.0';

if nargin<1
    error('A .hdf5 (or .hdf) file is needed as input.')
end

[path,filename,ext] = fileparts(hdf5file);

if ~strcmp(ext,'.hdf5') && ~strcmp(ext,'.hdf')
    error('The input file is not an .hdf5 or .hdf.')
end

datapar = h5read(hdf5file,'/Metadata/Data Parameters');
data    = h5read(hdf5file,'/Data/Table Layout');
exprpar = h5read(hdf5file,'/Metadata/Experiment Parameters');

exprparnames = cellstr(exprpar.name');
exprparvalues = cellstr(exprpar.value');

for ii = 1:length(exprparnames)
    matfile.metadata.software.experiment.(char(regexprep(exprparnames(ii),' ','_'))) = {exprparvalues{ii}};
end

aa = find(strcmp(exprparnames,'Cedar file name')==1);
bb = find(strcmp(exprparnames,'kindat')==1);
cc = find(strcmp(exprparnames,'start time')==1);
dd = find(strcmp(exprparnames,'end time')==1);

cedarfile = char(exprparvalues(aa,:));

kindat_values = char(exprparvalues(bb,:));
kindats = str2num(char(strsplit(kindat_values,', ')'));
nkindats = length(kindats);
evenkindat = num2str(max(kindats));

starttime = char(exprparvalues(cc,:));
endtime   = char(exprparvalues(dd,:));
starttime(11) = 'T'; starttime(20:22) = '';
endtime(11)   = 'T'; endtime(20:22)   = '';

pathparts = strsplit(cedarfile,filesep);
site = char(pathparts(end-2));
parnames = datapar.mnemonic';
npar = size(parnames,1);

year   = num2str(data.year(1));
month  = sprintf('%02d',data.month(1));
day    = sprintf('%02d',data.day(1));
hour   = sprintf('%02d',data.hour(1));
minute = sprintf('%02d',data.min(1));
second = sprintf('%02d',data.sec(1));
display(['The site is ' site ' (' year ') and contains kindat ' kindat_values])

recs = length(data.ut1_unix);    % # of records
intper_med = median(data.ut2_unix-data.ut1_unix);
if intper_med < 10
    name_strategy = 'ant';
else
    name_strategy = num2str(intper_med);
end
%matfile.metadata.software.experiment.intper_median = {num2str(intper_med)};

aa = find(strcmp(exprparnames,'instrument')==1);
if contains(char(exprparvalues(aa)),'Kiruna'), name_ant = 'kir'; 
elseif contains(char(exprparvalues(aa)),'Sodankyl'), name_ant = 'sod';
elseif contains(char(exprparvalues(aa)),'Svalbard'), name_ant = 'esr';
elseif contains(char(exprparvalues(aa)),'combined'), name_ant = 'esa'; %error('name_ant = esa: Abort!! Abort!!')
elseif contains(char(exprparvalues(aa)),'UHF'), name_ant = 'uhf';
else name_ant = 'vhf'; end

if kindats(1) >= 6800
    name_expr = 'gup';
else
    name_expr = ['cp' evenkindat(2) lower(char(96 + str2num(evenkindat(3:4))/2))];
end
matfile.metadata.software.experiment.name_expr = {name_expr}; 

datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_' name_strategy '@' name_ant];
storepath = fullfile(datapath,datafolder);
if exist(storepath)
   rmdir(storepath,'s');
end
mkdir(storepath);

Hdf5File = sprintf('%s%s',datafolder,'.hdf5');
MatFile = sprintf('%s%s',datafolder,'.mat');
% Hdf5File = [datafolder '.hdf5'];
% MatFile =  ['MAT_' year '-' month '-' day '_' name_expr '_' name_strategy '@' name_ant '.mat'];
hdffilename = fullfile(storepath,Hdf5File);
matfilename = fullfile(storepath,MatFile);
EISCAThdf5file = hdffilename;

%keyboard
GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file
[~,text] = xlsread(GuisdapParFile);     
parameters_list = text(:,5);   % list that includes all parameters and keep their positions from the excel arc
gupparameters_list = text(:,1);

if exist(hdffilename)==2, delete(hdffilename); end

parnames = lower(datapar.mnemonic');
npar = size(parnames,1);
Parsinfile_list = cell(npar,1);
for ii = 1:npar
    Parsinfile_list{ii} = deblank(parnames(ii,:));
end

if ~isempty(find(strcmp(Parsinfile_list,'sracf0')==1)) && ~isempty(find(strcmp(Parsinfile_list,'sfacf0')==1))
    nracf = length(find(cell2mat(strfind(Parsinfile_list,'racf'))==1));    % # of racf (racf1, racf2, ... , racfnracf)
    data.acf0 = data.sracf0.*data.sfacf0;
    aa = find(strcmp(Parsinfile_list,'sracf0')==1);
    bb = find(strcmp(Parsinfile_list,'sfacf0')==1);
    Parsinfile_list{aa}= 'acf';  
    data = rmfield(data,'sfacf0');
    for mm = 1:nracf
        evalc(['data.acf' num2str(mm) '= data.acf0.*complex(data.racf' num2str(mm) ',data.iacf' num2str(mm) ')']);
    end
    real_acf = [];
    imag_acf = [];
end

newrec = (data.recno(end)+1)/nkindats;

nrec = [];
for ii= 1:newrec    
    nrec_tmp = [];
    for jj = 1:nkindats
        nrec_tmp = [nrec_tmp length(find(data.recno+1==(ii-1)*nkindats+jj))];
    end
    nrec = [nrec; nrec_tmp];
end

if ~isfield(data,'range')
    data.range = height_to_range(data.gdalt,data.elm,1e3);
end
% Spatial description of datapoints for DataCite
loc = [data.elm data.azm data.range];
rr_lon = find(strcmp(exprparnames,'instrument longitude')==1);
rr_lat = find(strcmp(exprparnames,'instrument latitude')==1);
rr_alt = find(strcmp(exprparnames,'instrument altitude')==1);
RECloc = [str2num(exprparvalues{rr_lon}) str2num(exprparvalues{rr_lat}) str2num(exprparvalues{rr_alt})];
gg = zeros(length(data.elm),3);
for ss = 1:length(data.elm)
    gg(ss,:) = loc2gg(RECloc,loc(ss,:));
end
  
if nkindats>1
    cc       = find(data.kindat == str2num(evenkindat));
    cc_pp    = find(data.kindat == str2num(evenkindat)-1);
    gg_sp    = gg(cc,:);
    gg_sp_pp = gg(cc_pp,:);
else
    gg_sp = gg;
end

matfile.data.par0d     = [];
matfile.metadata.par0d = [];
matfile.data.par1d     = [];
matfile.metadata.par1d = [];
matfile.data.par2d     = [];
matfile.metadata.par2d = [];
matfile.data.utime     = [];
matfile.metadata.utime = [];

for ii = 1:nkindats
    aa = find(strcmp('nrec',gupparameters_list)==1);
    if ii==2
        aa = find(strcmp('nrec_pp',gupparameters_list)==1);
        matfile.data.par2d_pp = [];
        matfile.metadata.par2d_pp = [];     
    end
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
    info(2) = {[info{2} '(kindat=' num2str(kindats(ii)) ')']};
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(aa)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
    if length(unique(nrec(:,ii)))==1
        matfile.data.par0d = [matfile.data.par0d nrec(1,ii)];
        matfile.metadata.par0d = [matfile.metadata.par0d info'];
    elseif length(unique(nrec(:,ii)))>1
        matfile.data.par1d = [matfile.data.par1d nrec(:,ii)];
        matfile.metadata.par1d = [matfile.metadata.par1d info'];
    end
end

for ii = 1:npar
    if strcmp(char(Parsinfile_list(ii)),'nsampi')
        continue
    end
    
    aa = find(strcmp(char(Parsinfile_list(ii)),parameters_list)==1);
    if aa
        bb = strfind(Parsinfile_list{ii},'+');
        if bb
            parameterdata = data.([Parsinfile_list{ii}(1:bb-1) '0x2B']);   % Rename e.g. po+ --> po0x2B, because the data name is like that for some reason (This will only affect parameter names that end with '+')
        elseif strcmp(char(Parsinfile_list(ii)),'sfacf0')
            continue
        elseif strcmp(char(Parsinfile_list(ii)),'acf')   
            if nkindats>1
                cckind = find(data.kindat == str2num(evenkindat));   % data.kindat does not seem to exist when there is only 1 kindat for the experiment
            else cckind = 1:length(data.recno);
            end
            for dd = 0:nracf  
                acfdata_rec = data.([char(Parsinfile_list(ii)) num2str(dd)]);
                real_acf = [real_acf real(acfdata_rec(cckind))];
                imag_acf = [imag_acf imag(acfdata_rec(cckind))];
            end
            matfile.data.acf(1,:,:) = real_acf;
            matfile.data.acf(2,:,:) = imag_acf;
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
            if cell2mat(strfind(info(1),'h_'))==1
                info{1} = info{1}(3:end);
            end
            info{2} = [info{2} ', the real(1) and imaginary(2) parts of acf0 to acf' num2str(nracf) '.'];
            info{4} = ['acf0 = sracf0*sfacf0, racf1-racf' num2str(nracf) ' (real) and iacf1-iacf' num2str(nracf) ' (imaginary)'];
            info{5} = ['(3800,3900) 3801-38' num2str(nracf,'%02.f') ' (racf:s) and 3901-39' num2str(nracf,'%02.f') ' (iacf:s)'];
            info{6} = '60';
            matfile.metadata.acf = info';
            continue
        else
            parameterdata = data.(char(Parsinfile_list(ii)));
            if contains('gdalt range power',Parsinfile_list(ii)) 
                parameterdata = parameterdata*1000;   % km --> m, kW --> W
            end
            if strcmp(Parsinfile_list(ii),'nsamp') && isfield(data,'nsampi')
                parameterdata = parameterdata + data.nsampi;   % nsamp = nsamp+nsampi
            end
        end
        
        if length(unique(parameterdata))==1
            
            if strcmp('tfreq',char(Parsinfile_list(ii)))
                aa = aa(1);                                    % since there are two 'tfreq' in parameters_list there will be two values in aa. However, for old experiments it is always the first one in the list that is the correct metadata
            end
            
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
            if cell2mat(strfind(info(1),'h_'))==1
                info{1} = info{1}(3:end);
            end
            info(4) = {'-'};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(aa)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
             
            if strcmp(char(Parsinfile_list(ii)),'ut1_unix') || strcmp(char(Parsinfile_list(ii)),'ut2_unix')
                matfile.data.utime = [matfile.data.utime parameterdata(1)]; 
                matfile.metadata.utime = [matfile.metadata.utime info'];
            else
                matfile.data.par0d = [matfile.data.par0d single(parameterdata(1))];
                matfile.metadata.par0d = [matfile.metadata.par0d info'];
            end
            continue
        end
       
        if sum(isnan(parameterdata))==length(parameterdata)   % check if all data are NaN
            continue                                          % if so, skip to next iteration
        end
        
        for jj = 1:nkindats
            if nkindats>1
                cc = find(data.kindat == kindats(jj));
                parameterdata_kindat = parameterdata(cc);
            else
                parameterdata_kindat = parameterdata;
            end    
                        
            if sum(isnan(parameterdata_kindat))==length(parameterdata_kindat)   % check if all data (for a certain kindat) are NaN
                continue                                                        % if so, skip to next iteration
            end
                      
            if strcmp('range',char(Parsinfile_list(ii))) && jj == 1
                bb = aa; end
            if strcmp('range',char(Parsinfile_list(ii)))
                aa = bb(jj); end                                            % since there are two 'range' in parameters_list there will be two values in aa. This bit of code requires the 'normal range' (range) to be before 'pprange' in the Guisdap parameter list.  
            if strcmp('tfreq',char(Parsinfile_list(ii))), aa = aa(1); end   % since there are two 'tfreq' in parameters_list there will be two values in aa. However, for old experiments it is always the first one in the list that is the correct metadata
            
            if strcmp('vobi',char(Parsinfile_list(ii))),  aa = find(strcmp('vo',parameters_list)==1);  end
            if strcmp('dvobi',char(Parsinfile_list(ii))), aa = find(strcmp('dvo',parameters_list)==1); end
            if strcmp('nel',char(Parsinfile_list(ii))),   aa = find(strcmp('ne',parameters_list)==1);  parameterdata_kindat = 10.^parameterdata_kindat; end
            if strcmp('dnel',char(Parsinfile_list(ii))),  aa = find(strcmp('dne',parameters_list)==1); parameterdata_kindat = 10.^parameterdata_kindat; end
            if strcmp('col',char(Parsinfile_list(ii))),   aa = find(strcmp('co',parameters_list)==1);  parameterdata_kindat = 10.^parameterdata_kindat; end
            if strcmp('dcol',char(Parsinfile_list(ii))),  aa = find(strcmp('dco',parameters_list)==1); parameterdata_kindat = 10.^parameterdata_kindat; end
            if strcmp('snl',char(Parsinfile_list(ii))),   aa = find(strcmp('sn',parameters_list)==1);  parameterdata_kindat = 10.^parameterdata_kindat; end
            if strcmp('dsnl',char(Parsinfile_list(ii))),  aa = find(strcmp('dsn',parameters_list)==1); parameterdata_kindat = 10.^parameterdata_kindat; end
            if strcmp('popl',char(Parsinfile_list(ii))),  aa = find(strcmp('pop',parameters_list)==1); parameterdata_kindat = 10.^parameterdata_kindat; end
            
            start = 0;
            par_1d = [];
            for kk = 1:newrec
                par_rec = parameterdata_kindat(start+1:start+nrec(kk,jj));
                par_check1d(kk) = length(unique(par_rec));
                if par_check1d(kk) == 1
                    par_1d = [par_1d; par_rec(1)];
                end
                start = start + nrec(kk,jj);
            end
            
            
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
            
            if cell2mat(strfind(info(1),'h_'))==1
                info{1} = info{1}(3:end);
            end
            info(4) = {'-'};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(aa)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
            if (contains(char(Parsinfile_list(ii)),'racf') || contains(char(Parsinfile_list(ii)),'iacf')) && ~contains(char(Parsinfile_list(ii)),'racf0')
                info2split = strsplit(char(info(2)));
                info(2) = join(info2split(2:end));
            end
            
            if length(par_1d)==newrec 
                if strcmp(char(Parsinfile_list(ii)),'ut1_unix') || strcmp(char(Parsinfile_list(ii)),'ut2_unix')
                    matfile.data.utime = [matfile.data.utime par_1d]; 
                    matfile.metadata.utime = [matfile.metadata.utime info'];
                else
                    matfile.data.par1d = [matfile.data.par1d double(par_1d)]; 
                    matfile.metadata.par1d = [matfile.metadata.par1d info'];
                end
                break
            else
                if jj == 2
                    matfile.data.par2d_pp = [matfile.data.par2d_pp single(parameterdata_kindat)];
                    matfile.metadata.par2d_pp = [matfile.metadata.par2d_pp info'];
                else
                    matfile.data.par2d = [matfile.data.par2d single(parameterdata_kindat)];
                    matfile.metadata.par2d = [matfile.metadata.par2d info'];
                end
            end
        end
    end
end


% special treatment for cp6 experiments (kindat = '66xx')
kindatstr = num2str(kindats(1));
if strcmp(kindatstr(1:2),'66')
    cp6_faultpars   = {'te','dte','ti','dti','pm','dpm','po+','dpo+','ph+','dph+','co','dco'};
    cp6_correctpars = {'Ne_lag0+','dNe_lag0+','Ne_tp','dNe_tp','hw_lor','dhw_lor','hw_expfit','dhw_expfit','ampl','dampl','blev','dblev'};
    for ii = 1:length(cp6_faultpars)
        vv = find(strcmp(matfile.metadata.par2d(5,:),cp6_faultpars(ii)));
        if vv
            ww = find(strcmp(gupparameters_list,cp6_correctpars(ii))==1);
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(ww) ':E' num2str(ww)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(ww)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(ww)]))};
            matfile.metadata.par2d(:,vv) = info';
            if strcmp(cp6_faultpars(ii),'te') || strcmp(cp6_faultpars(ii),'dte') || strcmp(cp6_faultpars(ii),'ti') || strcmp(cp6_faultpars(ii),'dti')
                matfile.data.par2d(:,vv) = 10.^(matfile.data.par2d(:,vv)/1000);
            elseif strcmp(cp6_faultpars(ii),'pm') || strcmp(cp6_faultpars(ii),'dpm') || strcmp(cp6_faultpars(ii),'po+') || strcmp(cp6_faultpars(ii),'dpo+')
                matfile.data.par2d(:,vv) = matfile.data.par2d(:,vv)*1000/10;
            elseif strcmp(cp6_faultpars(ii),'ph+') || strcmp(cp6_faultpars(ii),'dph+')
                matfile.data.par2d(:,vv) = matfile.data.par2d(:,vv)*1000*1e6;
            elseif strcmp(cp6_faultpars(ii),'co') || strcmp(cp6_faultpars(ii),'dco')
                matfile.data.par2d(:,vv) = log10(matfile.data.par2d(:,vv))*1000*1e6;
            end
        end
    end
end

% add the RECloc data
cc1 = find(strcmp(exprparnames,'instrument latitude')==1);
cc2 = find(strcmp(exprparnames,'instrument longitude')==1);
cc3 = find(strcmp(exprparnames,'instrument altitude')==1);

%gupparameters_list = text(:,1);
if cc1, matfile.data.par0d = [matfile.data.par0d str2num(exprparvalues{cc1})];
    aa = find(strcmp('RECloc1',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
    info(4) = {'-'}; info(6:7) = {'0' num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
    matfile.metadata.par0d = [matfile.metadata.par0d info'];
end
if cc2, matfile.data.par0d = [matfile.data.par0d str2num(exprparvalues{cc2})];
    aa = find(strcmp('RECloc2',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
    info(4) = {'-'}; info(6:7) = {'0' num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
    matfile.metadata.par0d = [matfile.metadata.par0d info'];
end
if cc3, matfile.data.par0d = [matfile.data.par0d str2num(exprparvalues{cc3})*1000];
    aa = find(strcmp('RECloc3',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
    info(4) = {'-'}; info(6:7) = {'0' num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
    matfile.metadata.par0d = [matfile.metadata.par0d info'];
end

% TAI time (leapseconds)
[~,leaps] = timeconv(double(matfile.data.utime),'unx2tai');
if length(unique(leaps)) == 1
    if isfield(matfile.data,'par0d')
    	ll0 = length(matfile.data.par0d);
    else	
    	ll0 = 0;
    end
    matfile.data.par0d(ll0+1) = leaps(1);  
    a = find(strcmp('leaps',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par0d(:,ll0+1) = info';
else
    ll1 = length(matfile.data.par1d(1,:));
    matfile.data.par1d(:,ll1+1:ll1+2) = leaps;
    a = find(strcmp('leaps1',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par1d(:,ll1+1) = info';
    a = find(strcmp('leaps2',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par1d(:,ll1+2) = info';
end


nn = 0;
if exist('name_expr','var'); nn = nn + 1; 
    infoname(1) = {'name_expr'};
    infoname(2) = {name_expr};
    a = find(strcmp('name_expr',gupparameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_ant','var'); nn = nn + 1; 
    infoname(1) = {'name_ant'};
    infoname(2) = {name_ant};
    a = find(strcmp('name_ant',gupparameters_list)==1); 
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end

% DataCite
symbols = ['a':'z' 'A':'Z' '0':'9'];
strLength = 10;
nums = randi(numel(symbols),[1 strLength]);
randstr = symbols(nums);
PID = ['doi://eiscat.se/3a/' year month day hour minute second '/' randstr];
matfile.metadata.schemes.DataCite.Identifier = {PID};
matfile.metadata.schemes.DataCite.Creator = {name_ant};
matfile.metadata.schemes.DataCite.Title = {datafolder};
matfile.metadata.schemes.DataCite.Publisher = {'EISCAT Scientific Association'};
matfile.metadata.schemes.DataCite.ResourceType.Dataset = {'Level 3 Velocity'};
matfile.metadata.schemes.DataCite.Date.Collected = {[starttime '/' endtime]};
matfile.metadata.schemes.DataCite.PublicationYear = {year};

% Find the smallest box (4 corners and mid-point) to enclose the data.
% If area of convhull < 10-4 deg^2, define alla points as one (average)
% imag = 1 to plot the data and the corresponding box
im = 1;
[plonlat,PointInPol] = polygonpoints([gg_sp(:,2) gg_sp(:,1)],im);
matfile.metadata.schemes.DataCite.GeoLocation.PolygonLon = plonlat(:,1);
matfile.metadata.schemes.DataCite.GeoLocation.PolygonLat = plonlat(:,2);
if ~isempty(PointInPol)
    matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLon = PointInPol(1);
    matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLat = PointInPol(2);
end

if nkindats>1
    [plonlat,PointInPol] = polygonpoints([gg_sp_pp(:,2) gg_sp_pp(:,1)],im);
    matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLon = plonlat(:,1); 
    matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLat = plonlat(:,2);
    if ~isempty(PointInPol)
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLon = PointInPol(1);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLat = PointInPol(2);
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

software = 'https://git.eiscat.se/eiscat/on-an';
level2_link = [];
matfile.metadata.software.EISCAThdf5_ver = {hdf5ver};
matfile.metadata.software.software_link = {software};
matfile.metadata.level2_links = level2_link;
if exist('name_strategy')
    matfile.metadata.software.strategy = {'name_strategy'};
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

% Remove the 'Original parameter' (guisdap analysis parameter name) cells from
% the metadata since they are empty, and put empty values in 'Madrigal Id'
% and 'Identifier' to 0 instead
matfile.metadata.header = text(1,1:7)';
gupnamesindex = find(strcmp('Original parameter',matfile.metadata.header));
matfile.metadata.header(gupnamesindex) = [];
if isfield(matfile.metadata,'par0d')
    aa = find(cellfun('isempty',matfile.metadata.par0d(6,:)));    matfile.metadata.par0d(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par0d(7,:)));    matfile.metadata.par0d(7,aa)= {'0'};
    matfile.metadata.par0d(gupnamesindex,:) = [];
end
if isfield(matfile.metadata,'par1d')
    aa = find(cellfun('isempty',matfile.metadata.par1d(6,:)));    matfile.metadata.par1d(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par1d(7,:)));    matfile.metadata.par1d(7,aa)= {'0'};
    matfile.metadata.par1d(gupnamesindex,:) = [];
end
if isfield(matfile.metadata,'par2d')
    aa = find(cellfun('isempty',matfile.metadata.par2d(6,:)));    matfile.metadata.par2d(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par2d(7,:)));    matfile.metadata.par2d(7,aa)= {'0'};
    matfile.metadata.par2d(gupnamesindex,:) = [];
end
if isfield(matfile.metadata,'par2d_pp')
    aa = find(cellfun('isempty',matfile.metadata.par2d_pp(6,:))); matfile.metadata.par2d_pp(6,aa)= {'0'};
    aa = find(cellfun('isempty',matfile.metadata.par2d_pp(7,:))); matfile.metadata.par2d_pp(7,aa)= {'0'};
    matfile.metadata.par2d_pp(gupnamesindex,:) = [];
end

% Remove alt, lon and lat from par2d_pp

if isfield(matfile.metadata,'par2d_pp')
    a = find(strcmp('h',matfile.metadata.par2d_pp(1,:)));
    if a
        matfile.data.par2d_pp(:,a)     = [];
        matfile.metadata.par2d_pp(:,a) = [];
    end
    a = find(strcmp('lon',matfile.metadata.par2d_pp(1,:)));
    if a
        matfile.data.par2d_pp(:,a)     = [];
        matfile.metadata.par2d_pp(:,a) = [];
    end   
    a = find(strcmp('lat',matfile.metadata.par2d_pp(1,:)));
    if a
        matfile.data.par2d_pp(:,a)     = [];
        matfile.metadata.par2d_pp(:,a) = [];
    end
end

save(matfilename,'matfile')

% Generate an HDF5-file 
chunklim = 10;
sFields = fieldnames(matfile);
for sf = sFields.'
    group1 = ['/' char(sf)];
    tFields = fieldnames(matfile.(char(sf)));
    for tf = tFields.'
        if strcmp('data',char(sf)) && (strcmp('par0d',char(tf)) || strcmp('par1d',char(tf)) || strcmp('par2d',char(tf)) || strcmp('par2d_pp',char(tf)))
            npar  = length(matfile.data.(char(tf))(1,:));
            ndata = length(matfile.data.(char(tf))(:,1));
            if ge(ndata,chunklim) && ge(npar,chunklim), csize = [chunklim chunklim];
            elseif ge(ndata,chunklim), csize = [chunklim npar];
            elseif ge(npar,chunklim), csize = [ndata chunklim];
            else csize = [ndata npar]; end 
            h5create(hdffilename,['/' char(sf) '/' char(tf)],size([matfile.(char(sf)).(char(tf))]),'ChunkSize',csize,'Deflate',9,'Datatype','single');
            h5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
        elseif strcmp('data',char(sf)) && strcmp('acf',char(tf))
            acfsize = size(matfile.data.acf); 
            nrow   = acfsize(1);
            ncol   = acfsize(2);
            ndepth = acfsize(3);
            if ge(ncol,chunklim) && ge(ndepth,chunklim), csize = [nrow chunklim chunklim];
            elseif ge(ncol,chunklim),   csize = [nrow chunklim ndepth];
            elseif ge(ndepth,chunklim), csize = [nrow ncol chunklim];
            else, csize = [nrow nrow ncol]; end
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
