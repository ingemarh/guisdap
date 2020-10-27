% Generate a new EISCAT HDF5 file from an old HDF5-file fromated in Madrigal, called by EISCAT_hdf5.m

function [Storepath,EISCATvelhdf5file] = cedarvel2hdf5(hdf5file,datapath)

global path_GUP 

hdf5ver = hdfver;
%software = 'https://git.eiscat.se/eiscat/on-an';
level2_link = '';

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

%%% Check if velocity data exist
if ~isfield(data,'vipe')
    error(['Error: No velocity data in ' hdf5file])
end

exprparnames = cellstr(exprpar.name');
exprparvalues = cellstr(exprpar.value');

aa = find(strcmp(exprparnames,'Cedar file name')==1);
bb = find(strcmp(exprparnames,'kindat')==1);
cc = find(strcmp(exprparnames,'start time')==1);
dd = find(strcmp(exprparnames,'end time')==1);

cedarfile = char(exprparvalues(aa,:));

pathparts = strsplit(cedarfile,filesep);
site = char(pathparts(end-2));
parnames = lower(datapar.mnemonic');
npar = size(parnames,1);
Parsinfile_list = cell(npar,1);
for ii = 1:npar
    Parsinfile_list{ii} = deblank(parnames(ii,:));
end

%Remove data with strange time stamps
medtime = median(data.ut1_unix(:,1));
tt = find(data.ut1_unix(:,1)<medtime-1e6 | data.ut1_unix(:,1)>medtime+1e6);
if tt
    for pp = 1:npar
        data.(Parsinfile_list{pp})(tt) = [];
    end
end

year   = num2str(data.year(1));
month  = sprintf('%02d',data.month(1));
day    = sprintf('%02d',data.day(1));
hour   = sprintf('%02d',data.hour(1));
minute = sprintf('%02d',data.min(1));
second = sprintf('%02d',data.sec(1));

kindat_values = char(exprparvalues(bb,:));
kindats = str2num(char(strsplit(kindat_values,', ')'));
nkindats = length(kindats);
evenkindat = num2str(max(kindats));

starttime = char(exprparvalues(cc,:));
endtime   = char(exprparvalues(dd,:));
starttime(11) = 'T'; starttime(20:22) = '';
endtime(11)   = 'T'; endtime(20:22)   = '';

if contains(hdf5file,'.ar'), name_ant = 'kst';
else
    aa = find(strcmp(exprparnames,'instrument')==1);
    if contains(char(exprparvalues(aa)),'Kiruna'), name_ant = 'kir'; 
    elseif contains(char(exprparvalues(aa)),'Sodankyl'), name_ant = 'sod';
    elseif contains(char(exprparvalues(aa)),'Svalbard'), name_ant = 'esr';
    elseif contains(char(exprparvalues(aa)),'combined'), name_ant = 'esa';
    elseif contains(char(exprparvalues(aa)),'UHF'), name_ant = 'uhf';
    else name_ant = 'vhf'; end
end

GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file
[~,text] = xlsread(GuisdapParFile);     
parameters_list = text(:,5);   % list that includes all parameters and keep their positions from the excel arc
gupparameters_list = text(:,1);



for qq = 1:nkindats
    
    if exist('matfile','var')
        clear('matfile')
    end
    if exist('data_set','var')
        clear('data_set')
    end
    
    kindat_str = num2str(kindats(qq));

    if isfield(data,'kindat')
        ki = find(data.kindat == kindats(qq));
    else 
        ki = 1:length(data.ut1_unix);   % There is only one kindat for all data ('data.kindat' does not exist in this case)
    end
    if length(ki) == 1
	   continue
    end 

    for pp = 1:npar
        data_set.(deblank(Parsinfile_list{pp})) = data.(Parsinfile_list{pp})(ki);
    end

    for ii = 1:length(exprparnames)
        matfile.metadata.software.experiment.(char(regexprep(exprparnames(ii),' ','_'))) = {exprparvalues{ii}};
    end

    % Modify metadata if needed
    if nkindats > 1
        
        % kindat
        matfile.metadata.software.experiment.kindat = {num2str(kindats(qq))};
                
        % start_ & end_time
        dtstart = datetime(data_set.ut1_unix(1),'ConvertFrom','posixtime');
        dtend   = datetime(data_set.ut2_unix(end),'ConvertFrom','posixtime');
        matfile.metadata.software.experiment.start_time = {datestr(dtstart,'yyyy-mm-dd HH:MM:SS UT')};
        matfile.metadata.software.experiment.end_time   = {datestr(dtend,'yyyy-mm-dd HH:MM:SS UT')};
        
        % kind_of_data_file
        aa = find(kindats==kindats(qq));
        bb1 = strfind(char(matfile.metadata.software.experiment.kind_of_data_file),[' ' num2str(aa) ')']);
        cc = strfind(char(matfile.metadata.software.experiment.kind_of_data_file),['(code ' num2str(kindats(qq)) ')']);
        matfile.metadata.software.experiment.kind_of_data_file = {matfile.metadata.software.experiment.kind_of_data_file{1}(bb1+4:cc-2)};
    end

    intper_mean = subsetmean(data_set.ut2_unix-data_set.ut1_unix);        % mean integration time (of quartile range 2 and 3)
    intper_mean_str = num2str(round(intper_mean,3,'significant'));

    if kindats(qq) >= 6800
        name_expr = 'gup';
    else
        name_expr = ['cp' kindat_str(2) lower(char(96 + str2num(kindat_str(3:4))/2))];
    end
    matfile.metadata.software.experiment.name_expr = {name_expr}; 

    datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_V' intper_mean_str '@' name_ant];
    storepath = fullfile(datapath,datafolder);

    while exist(storepath)
        letters = 'a':'z';
        at = strfind(datafolder,'@'); 
        if length(datafolder(at+1:end)) == 3
            datafolder = [datafolder letters(2)];
        else
            letno = strfind(letters,storepath(end));
            datafolder = [datafolder(1:end-1) letters(letno+1)];
        end
        storepath = fullfile(datapath,datafolder);
    end
    mkdir(storepath);

    Hdf5File = [datafolder '.hdf5'];
    MatFile =  ['MAT_' year '-' month '-' day '_' name_expr '_V' intper_mean_str '@' name_ant '.mat'];
    hdffilename = fullfile(storepath,Hdf5File);
    matfilename = fullfile(storepath,MatFile);
    EISCATvelhdf5file(qq,:) = {hdffilename};
    Storepath(qq,:) = {storepath};
    
    if exist(hdffilename)==2, delete(hdffilename); end

    %%% Remove no-data (Vm = [0 0 0]), and create new data.*
    Vm = [data_set.vipn data_set.vipe data_set.vi6];
    MM = [];
    for mm = 1:length(data_set.vipn)
        if all(Vm(mm,:)==0)
            MM = [MM mm];
        end
    end
    if length(MM) == length(data_set.vipn)    % That is, if al Vm = 0
        rmdir(storepath)
        EISCATvelhdf5file(qq,:) = {''};
        Storepath(qq,:) = {''};
        continue
    end
    for ii = 1:npar
         data_set.(char(Parsinfile_list{ii}))(MM) = [];
    end
   
    matfile.data.par0d     = [];
    matfile.metadata.par0d = [];
    matfile.data.par1d     = [];
    matfile.metadata.par1d = [];
    matfile.data.utime     = [];
    matfile.metadata.utime = [];
 
    urecno = unique(data_set.recno);
    for nr = 1:length(urecno)
        rr = find(data_set.recno == urecno(nr));
        n_rec(nr) = length(rr);
    end

    aa = find(strcmp('h_nrec',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(aa)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
    if length(unique(n_rec)) == 1
        matfile.data.par0d = n_rec(1);
        matfile.metadata.par0d = [matfile.metadata.par0d info'];
    else
        matfile.data.par1d = n_rec;
        matfile.metadata.par1d = [matfile.metadata.par1d info'];
    end
    
    for ii = 1:npar
        aa = find(strcmp(char(Parsinfile_list(ii)),parameters_list)==1);
        if aa
            parameterdata = data_set.(char(Parsinfile_list(ii)));   
            if contains('gdalt range power',Parsinfile_list(ii))
                parameterdata = parameterdata*1000;   % km --> m, kW --> W
            elseif contains('bn be bd dvipn dvipe dvi6',Parsinfile_list(ii))
                continue
            end

            %%% 0d
            if length(unique(parameterdata))==1
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

            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);

            if cell2mat(strfind(info(1),'h_'))==1
                info{1} = info{1}(3:end);
            end
            info(4) = {'-'};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(aa)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};

            if strcmp(char(Parsinfile_list(ii)),'ut1_unix') || strcmp(char(Parsinfile_list(ii)),'ut2_unix')
                matfile.data.utime = [matfile.data.utime parameterdata]; 
                matfile.metadata.utime = [matfile.metadata.utime info'];
            else
                matfile.data.par1d = [matfile.data.par1d double(parameterdata)]; 
                matfile.metadata.par1d = [matfile.metadata.par1d info'];
            end
        end
    end

    %%% Velocities
    Vm = [data_set.vipe data_set.vipn data_set.vi6];        % [eastperp, northperp, antiparallel]
    dVm = [data_set.dvipe data_set.dvipn data_set.dvi6];
    B = [data_set.bn data_set.be data_set.bd];              % [north, east, down]
    Vg  = [];
    dVg = [];
    Vg_crossvar = [];
    for mm = 1:length(data_set.vipn)
        [vg,vgv] = Vm2Vg(Vm(mm,:),B(mm,:),dVm(mm,:));
        dvg = sqrt(diag(vgv));
        Vg  = [Vg;vg'];
        dVg = [dVg;dvg'];
        Vg_crossvar = [Vg_crossvar;vgv([2 6 3])];
    end

    %new_v  = {'vi1' 'vi2' 'vi3' 'dvi1' 'dvi2' 'dvi3' 'vi_crossvar_12' 'vi_crossvar_23' 'vi_crossvar_13'};
    new_v  = {'vi_east' 'vi_north' 'vi_up' 'dvi_east' 'dvi_north' 'dvi_up' 'vi_crossvar_12' 'vi_crossvar_23' 'vi_crossvar_13'};
    if ~isempty(matfile.data.par1d)
        ll1 = length(matfile.data.par1d(1,:));
    else
        ll1 = 0;
        matfile.metadata.par1d = {};
    end
    matfile.data.par1d(:,ll1+1:ll1+9) = [Vg dVg Vg_crossvar];

    for jj = 1:9
        a = find(strcmp(new_v{jj},gupparameters_list)==1);
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.par1d(:,ll1+jj) = info';
    end

    %%% TAI time (leapseconds)
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
    matfile.metadata.schemes.DataCite.ResourceType.Dataset = {'Level 4a Derived ionospheric data'};
    matfile.metadata.schemes.DataCite.Date.Collected = {[starttime '/' endtime]};
    matfile.metadata.schemes.DataCite.PublicationYear = {year};

    % Find the smallest box (4 corners and mid-point) to enclose the data.
    % If area of convhull < 10-4 deg^2, define alla points as one (average)
    % imag = 1 to plot the data and the corresponding box
    %gg_sp = [data.glon(ki) data.gdlat(ki)];
    im = [];
    [plonlat,PointInPol] = polygonpoints([data_set.glon data_set.gdlat],im);

    matfile.metadata.schemes.DataCite.GeoLocation.PolygonLon = plonlat(:,1);
    matfile.metadata.schemes.DataCite.GeoLocation.PolygonLat = plonlat(:,2);

    if ~isempty(PointInPol)
        matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLon = PointInPol(1);
        matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLat = PointInPol(2);
    end

    level2_link = '';
    %matfile.metadata.software.software_link = {software};
    matfile.metadata.software.EISCAThdf5_ver = {hdf5ver};
    matfile.metadata.software.strategy = {intper_mean_str};
    if ~isempty(level2_link)
        matfile.metadata.software.level2_links = {level2_link};
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

    save(matfilename,'matfile')

    % Generate an HDF5-file 
    chunklim = 10;
    sFields = fieldnames(matfile);
    for sf = sFields.'
        group1 = ['/' char(sf)];
        tFields = fieldnames(matfile.(char(sf)));
        for tf = tFields.'
            if strcmp('data',char(sf)) && (strcmp('par0d',char(tf)) || strcmp('par1d',char(tf)) || strcmp('par2d',char(tf)) || strcmp('par2d_pp',char(tf)))
                npars  = length(matfile.data.(char(tf))(1,:));
                ndata = length(matfile.data.(char(tf))(:,1));
                if ge(ndata,chunklim) && ge(npars,chunklim), csize = [chunklim chunklim];
                elseif ge(ndata,chunklim), csize = [chunklim npars];
                elseif ge(npars,chunklim), csize = [ndata chunklim];
                else csize = [ndata npars]; end 
                h5create(hdffilename,['/' char(sf) '/' char(tf)],size([matfile.(char(sf)).(char(tf))]),'ChunkSize',csize,'Deflate',9,'Datatype','single');
                h5write(hdffilename,['/' char(sf) '/' char(tf)],single(matfile.(char(sf)).(char(tf))));
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
                h5write(hdffilename,['/' char(sf) '/' char(tf)],single(matfile.(char(sf)).(char(tf))));
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
                h5write(hdffilename,['/' char(sf) '/' char(tf)],matfile.(char(sf)).(char(tf)));
            end   
        end
    end
end