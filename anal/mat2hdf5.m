% Generate an EISCAT HDF5-file from mat-files generated in a Guisdap analysis

function [Storepath,EISCAThdf5file,nvecvel] = mat2hdf5(matpath,datapath,addfigs,addnotes) 

global path_GUP result_path name_ant

name_ant = [];

if nargin<4, addnotes = []; else addnotes = 1; end 
if nargin<3, addfigs = []; else addfigs = 1; end 
if nargin==1, error('Not enough input parameters, path to matfiles folder and path to datastore folder needed'); end
if nargin<1
    matpath  = result_path;
    datapath = result_path;
end
if isstring(datapath)
    datapath = char(datapath);    % need to be char class
end

hdf5ver = hdfver;
software = 'https://git.eiscat.se/cvs/guisdap9';
level2_link = '';
if strcmp(matpath(end),'/')
    matpath = matpath(1:end-1);   % remove last / in order to extract matfolder correctly
end
[~,matfolder] = fileparts(matpath);
matpath = [matpath '/'];          % in order to make 'getfilelist.m' work

% Chech for vector velocity data
filelist_all = dir(matpath);
filelist_all = filelist_all(endsWith({filelist_all.name},'.mat'));   % consider only .mat-files
for jj = 1:length(filelist_all)
    [~,~,ext] = fileparts(filelist_all(jj).name);
    if strcmp(ext,'.bz2')
        unix(['bunzip2 ' fullfile(filelist_all(jj).folder,filelist_all(jj).name)]);
    end
end
filelist = getfilelist(matpath);

qq = 0;
% Chech for vector velocity data
if length(filelist_all) > length(filelist)
    for kk = 1:length(filelist_all)
        matfilecheck = fullfile(filelist_all(kk).folder,filelist_all(kk).name);
        if ~isempty(who('-file',matfilecheck,'Vg'))
            qq = qq + 1; 
            [storepath,file_EISCAThdf5] = matvecvel2hdf5(matfilecheck,datapath);
            EISCAThdf5file(qq,:) = file_EISCAThdf5;
            Storepath(qq,:) = storepath;
        end
    end
end
nvecvel = qq;   % number vecvel-experiments

rec  = length(filelist);
TT = []; intper_vec = []; pars_recs = []; h_sd = []; ntstamps_sd = [];
for tt = 1:rec
    if exist('r_sd','var')
        clear('r_sd')
    end
    try
        load(filelist(tt).fname)
    catch
        continue 
    end
    TT = [TT tt];
    intper_vec_rec = posixtime(datetime(r_time(2,1:6)))-posixtime(datetime(r_time(1,1:6)));
    intper_vec = [intper_vec; intper_vec_rec];
    pars_recs = [pars_recs length(r_param(1,:))];
    if exist('r_sd','var')
        ntstamps_sd = [ntstamps_sd size(r_sd,1)];
        h_sd = [h_sd; r_sd];
    else
        ntstamps_sd = [ntstamps_sd 0];
    end
end

if length(TT)<rec
    filelist = filelist(TT);       % matfile(s) that was(were) corrupt or could not be read is(are) removed 
end

if isempty(filelist) || length(filelist) == 1    % Ignore if empty or if there is only one(1) record
    qq = qq + 1;
    EISCAThdf5file(qq,:) = {''};
    Storepath(qq,:) = {''};
    pars_recs = [];
end

% Remove isolated record(s) with another number of parameters in r_param
% than the direct previous and direct subsequent record
UUind = [];
dpars_recs = diff(pars_recs);
for hj = 1:length(pars_recs)
    if hj == 1 
        if dpars_recs(hj) ~= 0      % first record has another number of parameters than second
            UUind = hj;
        end
    elseif hj == length(pars_recs) 
        if dpars_recs(hj-1) ~= 0    % last record has another number of parameters than second last
            UUind = hj;
        end
    elseif dpars_recs(hj-1) ~= 0
        if dpars_recs(hj) ~= 0
            UUind = [UUind hj];
        end
    end
end 
filelist(UUind)  = [];
pars_recs(UUind) = [];

% check where # of pars change + end index
pci = [find(diff(pars_recs) ~= 0) length(filelist)];
for d = 1:length(pci)
    if d == 1
        rec(d) = pci(d);
    else
        rec(d) = pci(d)-pci(d-1);
    end
end
if any(pci == [0,1]), pci = []; end 

for rr = 1:length(pci)
    
    if exist('matfile','var')
        clear matfile
    end
    
    qq = qq + 1;
    
    if rr == 1
        startexpr = 1;
        start_sd  = 1;
    else
        startexpr = pci(rr-1) + 1;
        if ~isempty(h_sd)
            start_sd  = sum(ntstamps_sd(1:startexpr)) + 1;
        end
    end
    stopexpr = pci(rr);
    Filelist = filelist(startexpr:stopexpr);
    Intper_vec = intper_vec(startexpr:stopexpr);
    
    % space debris data
    if ~isempty(h_sd)
        stop_sd = sum(ntstamps_sd(1:stopexpr));    
        h_Sd = h_sd(start_sd:stop_sd,:);      
    end
    
    % store the gfd content and check Tsys
    load(Filelist(1).fname)
    if exist('r_Tsys','var'),   nTsys   = length(r_Tsys);   end
    if exist('r_m0','var'),     nm0     = length(r_m0);     end
    if exist('r_code','var'),   ncode   = length(r_code);   end
    if exist('r_om0','var'),    nom0    = length(r_om0);    end
    if exist('r_fradar','var'), nfradar = length(r_fradar); end
    if exist('r_gain','var'),   ngain   = length(r_gain);   end

    s = [];
    starttime = [];  endtime = [];
    intper_mean = []; intper_mean_str = [];
    if exist('r_gfd','var')
        gfd_Fields = fieldnames(r_gfd);
        for gf = gfd_Fields.'   
            if strcmp(gf,'extra')
                extra=r_gfd.extra;
                for r = 1:size(extra,1)
                    if contains(extra(r,:),'%') || isempty(deblank(extra(r,:)))
                        s = [s r];
                    end
                end
                extra(s,:)=[];
                if ~isempty(extra) && size(extra,1) == 1
                    matfile.metadata.software.gfd.extra = {deblank(extra)};
                elseif ~isempty(extra) 
                    matfile.metadata.software.gfd.extra = {row([extra ones(size(extra,1))*'#']')};
                else
                    matfile.metadata.software.gfd.extra = {'None'};
                end
            elseif ~ischar(r_gfd.(char(gf)))
                matfile.metadata.software.gfd.(char(gf)) = {num2str(r_gfd.(char(gf)))};
            else
                matfile.metadata.software.gfd.(char(gf)) = {r_gfd.(char(gf))};
            end
        end
        if length(pci) == 1
            starttime = datestr(r_gfd.t1,'yyyy-mm-ddTHH:MM:SS');
            endtime   = datestr(r_gfd.t2,'yyyy-mm-ddTHH:MM:SS');
            intper_mean = subsetmean(r_gfd.intper);
            intper_mean_str = num2str(round(intper_mean,3,'significant'));
        end
    elseif exist(fullfile(matpath,'.gup'),'file')    
        load('-mat',fullfile(matpath,'.gup'));
        if exist('name_expr','var'),    matfile.metadata.software.gfd.name_expr      = {name_expr};           end
        if exist('expver','var'),       matfile.metadata.software.gfd.expver         = {num2str(expver)};     end
        if exist('siteid','var'),       matfile.metadata.software.gfd.siteid         = {num2str(siteid)};     end
        if exist('data_path','var'),    matfile.metadata.software.gfd.data_path      = {data_path};           end
        if exist('result_path','var'),  matfile.metadata.software.gfd.result_path    = {result_path};         end 
        if exist('intper','var'),       matfile.metadata.software.gfd.intper         = {num2str(intper)};  
            if ~exist('intper_mean','var'), intper_mean = subsetmean(intper); intper_mean_str = num2str(round(intper_mean,3,'significant')); end
        end
        if exist('t1','var'),           matfile.metadata.software.gfd.t1             = {num2str(t1)};         end
        if exist('t2','var'),           matfile.metadata.software.gfd.t2             = {num2str(t2)};         end
        if exist('rt','var'),           matfile.metadata.software.gfd.rt             = {num2str(rt)};         end
        if exist('figs','var'),         matfile.metadata.software.gfd.figs           = {num2str(figs)};       end
        if exist('path_exps','var'),    matfile.metadata.software.gfd.path_exps      = {path_exps};           end
        if exist('extra','var')
            for r = 1:size(extra,1)
                if contains(extra(r,:),'%') || isempty(deblank(extra(r,:)))
                    s = [s r];
                end
            end
            extra(s,:)=[];
            if ~isempty(extra) && size(extra,1) == 1
                matfile.metadata.software.gfd.extra = {deblank(extra)};
            elseif ~isempty(extra)
                matfile.metadata.software.gfd.extra = {row([extra ones(size(extra,1))*'#']')};
            else
                matfile.metadata.software.gfd.extra = {'None'};
            end        
        end
        if length(pci) == 1
            starttime = datestr(t1,'yyyy-mm-ddTHH:MM:SS');
            endtime   = datestr(t2,'yyyy-mm-ddTHH:MM:SS');
        end
    end
    
    if isempty(starttime) || size(starttime,1)>1
        load(Filelist(1).fname)
        starttime = datestr(r_time(1,:),'yyyy-mm-ddTHH:MM:SS');
        if length(pci) > 1
            matfile.metadata.software.gfd.t1 = {num2str([r_time(1,1:5) round(r_time(1,end))])};
        end
    end
    if isempty(endtime) || size(endtime,1)>1
        load(Filelist(end).fname)
        endtime = datestr(r_time(2,:),'yyyy-mm-ddTHH:MM:SS');
        if length(pci) > 1
            matfile.metadata.software.gfd.t2 = {num2str([r_time(2,1:5) round(r_time(2,end))])};
        end
    end
    
    if isempty(intper_mean)
        intper_mean = subsetmean(Intper_vec);
    end
    if isempty(intper_mean_str)
        intper_mean_str = num2str(round(intper_mean,3,'significant'));
    end

    if ~exist('name_strategy','var')
        if contains(matfolder,'scan')
            name_strategy = 'scan';
        elseif intper_mean == 0
            name_strategy = 'ant';
        elseif intper_mean < 0
            name_strategy = ['ant' num2str(-round(intper_mean,3,'significant'))];
        else 
            name_strategy = intper_mean_str;
        end
    end

    gg_sp = [];
    for tt = 1:rec(rr)
        load(Filelist(tt).fname)
        loc(1:2) = [r_el r_az];
        gg_rec = [];
        for uu = 1:length(r_range)
            loc(3) = r_range(uu);
            gg_point = loc2gg(r_RECloc,loc);
            gg_rec = [gg_rec; gg_point];
        end
        gg_sp = [gg_sp; gg_rec];
    end

    gg_sp_pp = [];
    if exist('r_pprange','var')
        for tt = 1:rec(rr)
            load(Filelist(tt).fname)
            loc(1:2) = [r_el r_az];
            gg_rec = [];
            for uu = 1:length(r_pprange)
                loc(3) = r_pprange(uu);
                gg_point = loc2gg(r_RECloc,loc);
                gg_rec = [gg_rec; gg_point];
            end
            gg_sp_pp = [gg_sp_pp; gg_rec];
        end
    end

    year = num2str(r_time(1,1));
    month = sprintf('%02d',r_time(1,2));
    day = sprintf('%02d',r_time(1,3));

    hour   = sprintf('%02d',r_time(1,4));
    minute = sprintf('%02d',r_time(1,5));
    second = sprintf('%02.f',r_time(1,6));

    if strcmp(matpath(end),'/')
        matpath = matpath(1:end-1);
    end

    %[~,matfolder] = fileparts(matpath);
    dd_  = strfind(matfolder,'_');
    ddat = strfind(matfolder,'@');
    dd = sort([dd_ ddat]);
    if length(dd)==4
        name_expr_more = [matfolder(dd(2)+1:dd(3)-1) '_'];
    else
        name_expr_more = [];
    end
    ant = matfolder(dd(end)+1:end);

    if isempty(name_ant) 
       switch name_site
           case 'L', if contains('32m 42m',ant), name_ant = ant; else name_ant = 'esr'; end
           case 'K', name_ant = 'kir';
           case 'T', name_ant = 'uhf';
           case 'S', name_ant = 'sod';
           case 'V', name_ant = 'vhf';
           case 'Q', name_ant = 'quj';
       end
    else
        name_ant = lower(name_ant);
    end

    datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_' name_expr_more name_strategy '@' name_ant];
    storepath = fullfile(datapath,datafolder);

    while exist(storepath)
        endnum = str2double(datafolder(end));
        if isnan(endnum)
            datafolder = [datafolder(1:end) num2str(1)];
        else
            datafolder = [datafolder(1:end-1) num2str(endnum+1)];            
        end
        storepath = fullfile(datapath,datafolder);
    end

    Hdf5File = sprintf('%s%s',datafolder,'.hdf5');
    MatFile = sprintf('%s%s',datafolder,'.mat');
    hdffilename = fullfile(storepath,Hdf5File);
    matfilename = fullfile(storepath,MatFile);
    Storepath(qq,:) = {storepath};
    EISCAThdf5file(qq,:) = {hdffilename};
    GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file

    if exist(hdffilename)==2, delete(hdffilename); end

    [~,text] = xlsread(GuisdapParFile);
    parameters_list = text(:,1);    % list that includes all Guisdap parameters and keep their positions from the excel arc

    matfile.metadata.header= text(1,1:7)';

    % unixtime
    matfile.data.utime = [];
    leaps = [];
    ttt = 0;
    a = find(strcmp('h_time',parameters_list)==1);
    for jj = 1:2, ttt = ttt + 1;
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
        matfile.metadata.utime(:,ttt) = info';
    end

    % UTC time
    for jj = 1:rec(rr)
         load(Filelist(jj).fname)
         [utime_rec,leap] = timeconv([r_time(1,:);r_time(2,:)],'utc2unx');
         matfile.data.utime = [matfile.data.utime;utime_rec'];
         leaps = [leaps;leap'];
    end

    load(Filelist(jj).fname)
    nn = 0;
    if exist('name_expr','var'); nn = nn + 1; 
        infoname(1) = {'name_expr'};
        infoname(2) = {name_expr};
        a = find(strcmp('name_expr',parameters_list)==1);                
        [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
        infoname(3) = infodesc;
        matfile.metadata.names(:,nn) = infoname';
    end
    if exist('name_site','var'); nn = nn + 1; 
        infoname(1) = {'name_site'};
        infoname(2) = {name_site};
        a = find(strcmp('name_site',parameters_list)==1);                
        [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
        infoname(3) = infodesc;
        matfile.metadata.names(:,nn) = infoname';
    end
    if exist('name_ant','var'); nn = nn + 1; 
        infoname(1) = {'name_ant'};
        infoname(2) = {lower(name_ant)};
        a = find(strcmp('name_ant',parameters_list)==1); 
        [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
        infoname(3) = infodesc;
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

    [h_Magic_const, h_az, h_el, h_Pt, h_SCangle, h_XMITloc, h_RECloc, h_nrec, h_ppnrec, ...
    h_Tsys, h_code, h_om0, h_m0, h_phasepush, h_Offsetppd, h_gain, h_fradar, ...
    h_h, h_range, h_param, h_error, h_apriori, h_apriorierror, h_status, h_dp_first, ...
    h_dp, h_ddp, h_apr, h_aprerr, h_res, h_w, h_pprange, h_pp, h_pperr, h_ppw] = deal([]);

    unicheck_om0    = zeros(rec(rr),1);
    unicheck_gain   = zeros(rec(rr),1);
    unicheck_fradar = zeros(rec(rr),1);

    for jj = 1:rec(rr)
        load(Filelist(jj).fname)
        if exist('r_Magic_const','var'), h_Magic_const = [h_Magic_const; r_Magic_const]; end
        if exist('r_az','var'), h_az = [h_az; col(r_az)]; end
        if exist('r_el','var'), h_el = [h_el; col(r_el)]; end
        if exist('r_Pt','var'), h_Pt = [h_Pt; col(r_Pt)]; end
        if exist('r_SCangle','var'), h_SCangle = [h_SCangle; col(r_SCangle)]; end
        if exist('r_XMITloc','var')
            r_XMITloc(3) = r_XMITloc(3)*1000; 
            h_XMITloc = [h_XMITloc; row(r_XMITloc)]; end
        if exist('r_RECloc','var')
            r_RECloc(3) = r_RECloc(3)*1000; 
            h_RECloc = [h_RECloc; row(r_RECloc)]; end
        if exist('r_Tsys','var')
            if length(r_Tsys) < nTsys
                r_Tsys(length(r_Tsys)+1:nTsys) = NaN;       % if r_Tsys is shorter: fill it up
            elseif length(r_Tsys) > nTsys
                h_Tsys(:,nTsys+1:length(r_Tsys)) = NaN;     % if r_Tsys is longer: fill up h_Tsys
                nTsys = length(r_Tsys);
            end
            h_Tsys = [h_Tsys; row(r_Tsys)]; end
        if exist('r_code','var')
            if length(r_code) < ncode
                r_code(length(r_code)+1:ncode) = NaN;       % if r_code is shorter: fill it up
            elseif length(r_code) > ncode
                h_code(:,ncode+1:length(r_code)) = NaN;     % if r_code is longer: fill up h_code
                ncode = length(r_code);
            end
            h_code = [h_code; r_code];
        end
        if exist('r_m0','var')
            h_m0 = [h_m0; r_m0];
            if exist('r_param','var')        
                if length(r_param(1,:)) == 5 && ~isempty(r_m0)     % if ion composition is missing in r_param
                    dpO = find(r_m0 == 16);
                    if isempty(dpO) || ~exist('r_dp')
                        %display('No ion composition data available.')
                    else
                        if length(r_m0) < 3
                            for iidp = 1:length(r_m0)
                                if iidp == dpO
                                    R_dp(:,iidp)  = col(r_dp);
                                else
                                    R_dp(:,iidp)  = 1-col(r_dp);
                                end
                            end
                        else
                            R_dp = col(r_dp);
                        end
                        R_ddp    = zeros(size(R_dp));
                        R_apr    = nan(size(R_dp));
                        R_aprerr = R_apr;
                        
                        h_dp     = [h_dp; R_dp];
                        h_ddp    = [h_ddp; R_ddp];
                        h_apr    = [h_apr; R_apr];
                        h_aprerr = [h_aprerr; R_aprerr];
                    end
                    clear('R_dp')
                else
                    r_dp_first  = 1 - sum(r_param(:,6:4+length(r_m0)),2);              % calculate first dp (corresponding to r_m0(1))
                    r_ddp_first = sum(r_error(:,6:4+length(r_m0)),2);                  % calculate first dp error (corresponding to r_m0(1))
                    r_aprdp_first = 1 - sum(r_apriori(:,6:4+length(r_m0)),2);          % calculate first dp apriori (corresponding to r_m0(1))
                    r_aprdp_err_first = sum(r_apriorierror(:,6:4+length(r_m0)),2);     % calculate first dp error (corresponding to r_m0(1))
                    h_dp_first = [h_dp_first; col(r_dp_first) col(r_ddp_first) col(r_aprdp_first) col(r_aprdp_err_first)];
                end
            end
        end
        if exist('r_phasepush','var'), h_phasepush = [h_phasepush; r_phasepush]; end
        if exist('r_Offsetppd','var'), h_Offsetppd = [h_Offsetppd; r_Offsetppd/1e6]; end     % us --> s
        if exist('r_om0','var')
            if length(unique(r_om0)) == 1, unicheck_om0(jj) = 1; end
            if length(r_om0) < nom0
                r_om0(length(r_om0)+1:nom0) = NaN;           % if r_om0 is shorter: fill it up
            elseif length(r_om0) > nom0
                h_om0(:,nom0+1:length(r_om0)) = NaN;         % if r_om0 is longer: fill up h_om0
                nom0 = length(r_om0);
            end
            h_om0 = [h_om0; row(r_om0)]; end
        if exist('r_gain','var')
            if length(unique(r_gain)) == 1, unicheck_gain(jj) = 1; end
                if length(r_gain) < ngain
                    r_gain(length(r_gain)+1:ngain) = NaN;          % if r_mo0 is shorter: fill it up
                elseif length(r_gain) > ngain
                    h_gain(:,ngain+1:length(r_gain)) = NaN;        % if r_mo0 is longer: fill up h_mo0
                    ngain = length(r_gain);
                end
                h_gain = [h_gain; row(r_gain)]; end
        if exist('r_fradar','var')
            if length(unique(r_fradar)) == 1, unicheck_fradar(jj) = 1; end
                if length(r_fradar) < nfradar
                    r_fradar(length(r_fradar)+1:nfradar) = NaN;          % if r_fradar is shorter: fill it up
                elseif length(r_fradar) > nfradar
                    h_fradar(:,nfradar+1:length(r_fradar)) = NaN;        % if r_fradar is longer: fill up h_fradar
                    nfradar = length(r_fradar);
                end
                h_fradar = [h_fradar; row(r_fradar)]; end
        if exist('r_h','var')
            h_nrec = [h_nrec; length(r_h)];
            h_h = [h_h; col(r_h)*1000]; end
        if exist('r_range','var'), h_range = [h_range; col(r_range)*1000]; end
        if exist('r_res','var'), h_res = [h_res; r_res]; end
        if exist('r_status','var')
    %         if ~isempty(r_status) && length(r_status) ~= length(r_h), keyboard, end
    %             r_status = 2*ones(size(r_h)); end    % put 'no fit' = 2 for whole record, because of wrong r_status record length
            h_status = [h_status; r_status]; end
        if exist('r_w','var')
            if size(r_w,2) == length(r_h), r_w = r_w'; end    
            h_w = [h_w; r_w*1000]; end
        if exist('r_param','var')
            h_param = [h_param; r_param]; end
        if exist('r_error','var')    
            h_error = [h_error; r_error]; end
        if exist('r_apriori','var')
            if length(h_param(1,:)) == 5
                r_apriori = r_apriori(:,1:5);
            end
            h_apriori = [h_apriori; r_apriori]; end 
        if exist('r_apriorierror','var')
            if length(h_param(1,:)) == 5
                r_apriorierror = r_apriorierror(:,1:5);
            end
            h_apriorierror = [h_apriorierror; r_apriorierror]; end 
        if exist('r_pprange','var')
            h_ppnrec  = [h_ppnrec; length(r_pprange)];
            h_pprange = [h_pprange; col(r_pprange)*1000]; end
        if exist('r_pperr','var'), h_pperr = [h_pperr; r_pperr]; end
        if exist('r_pp','var'), h_pp = [h_pp; r_pp]; end
        if exist('r_ppw','var'), h_ppw = [h_ppw; r_ppw*1000]; end
    end
    if length(r_param(1,:)) == 5 && isempty(h_dp)
        display('No ion composition data available.')
    end
        
    if ~any(unicheck_om0==0),    h_om0    = h_om0(:,1); end
    if ~any(unicheck_gain==0),   h_gain   = h_gain(:,1); end
    if ~any(unicheck_fradar==0), h_fradar = h_fradar(:,1); end

    if length(h_pp)    ~= length(h_pprange), h_pp    = []; end
    if length(h_pperr) ~= length(h_pprange), h_pperr = []; end
    if length(h_ppw)   ~= length(h_pprange), h_ppw   = []; end

    if exist('r_m0','var')
        if length(h_param(1,:)) > 5
            h_param        = [h_param(:,1:5) h_dp_first(:,1) h_param(:,6:end)];
            h_error        = [h_error(:,1:5) h_dp_first(:,2) h_error(:,6:end)];
            h_apriori      = [h_apriori(:,1:5) h_dp_first(:,3) h_apriori(:,6:end)];
            h_apriorierror = [h_apriorierror(:,1:5) h_dp_first(:,4) h_apriorierror(:,6:end)];
        elseif length(h_param(1,:)) == 5    
            h_param        = [h_param h_dp];
            h_error        = [h_error(:,1:5) h_ddp h_error(:,6:end)];
            h_apriori      = [h_apriori h_apr];
            h_apriorierror = [h_apriorierror h_aprerr];
            if length(h_m0(1,:)) > 2
                hh_m0 = h_m0(1,:);
                hh_m0(dpO) = [];
                h_m0 = h_m0(:,dpO);    % keeping only O+
                warning(['No ion composition data available for mi = ' regexprep(num2str(hh_m0),'\s+',', ')])
            end
        end
    end

    h_error = h_error.^2;     % error --> variance

    matfile.data.par1d    = [h_Magic_const h_az h_el h_Pt h_SCangle h_XMITloc ...
        h_RECloc h_Tsys h_code h_om0 h_m0 h_phasepush h_Offsetppd h_gain h_fradar h_nrec h_ppnrec];
    matfile.data.par2d    = [h_h h_range h_param h_error h_apriori...
        h_apriorierror h_status h_res h_w];
    matfile.data.par2d_pp = [h_pprange h_pp h_pperr h_ppw];


    % 0d and 1d parameters 
    parameters_1d = {'h_Magic_const' 'h_az' 'h_el' 'h_Pt' 'h_SCangle'...
        'h_XMITloc' 'h_RECloc' 'h_Tsys' 'h_code' 'h_om0' 'h_m0' 'h_phasepush'...
        'h_Offsetppd' 'h_gain' 'h_fradar' 'h_nrec' 'h_ppnrec'};
    % 2d parameters 
    parameters_2d = {'h_h' 'h_range' 'h_param' 'h_error' 'h_apriori'...
        'h_apriorierror' 'h_status' 'h_res' 'h_w'};
    % 2d_pp parameters 
    parameters_2dpp = {'h_pprange' 'h_pp' 'h_pperr' 'h_ppw'};

    nn1 = 0;
    for ii = 1:length(parameters_1d)
        h_name1 = char(parameters_1d(ii));
        if isempty(eval(h_name1))
            continue
        elseif strcmp(h_name1,'h_XMITloc')
            a = find(strcmp(h_name1,parameters_list)==1);
            for jj = 1:3; nn1 = nn1+1; 
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
                info(1) = {[h_name1(3:end) num2str(jj)]};
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
                matfile.metadata.par1d(:,nn1) = info';
            end
        elseif strcmp(h_name1,'h_RECloc') 
            a = find(strcmp(h_name1,parameters_list)==1);
            for jj = 1:3; nn1 = nn1+1; 
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
                info(1) = {[h_name1(3:end) num2str(jj)]};
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
                matfile.metadata.par1d(:,nn1) = info';
            end
        else
            a = find(strcmp(h_name1,parameters_list)==1);             
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
            info(1) = {h_name1(3:end)};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))}; 
            cols = size(eval(h_name1),2);
            if cols > 1
                for jj = 1:cols; nn1 = nn1+1;
                    info(1) = {[h_name1(3:end) num2str(jj)]};  
                    matfile.metadata.par1d(:,nn1) = info';
                end
            else
                nn1 = nn1+1;
                matfile.metadata.par1d(:,nn1) = info';
            end
        end
    end     

    nn2 = 0;
    for ii = 1:length(parameters_2d)
        h_name2 = char(parameters_2d(ii));
        if isempty(eval(h_name2))
            continue
        end
        cols = size(eval(h_name2),2);
        if cols==1
            nn2 = nn2 + 1;
            a = find(strcmp(h_name2,parameters_list)==1);
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
            info(1) = {h_name2(3:end)};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
            matfile.metadata.par2d(:,nn2) = info';
        elseif strcmp(h_name2,'h_param') || strcmp(h_name2,'h_error') || strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror')
            a = find(strcmp(h_name2,parameters_list)==1);
            if strcmp(h_name2,'h_error')
                a = find(strcmp('h_diagvariance',parameters_list)==1);
            end
            if strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror')
                nump = length(h_apriori(1,:))-1;                % -1 because of the extra content parameter (corresponding to r_m0(1)) included in r_param etc
            else
                nump = length(h_param(1,:))-1;                  % -1 because of the extra content parameter (corresponding to r_m0(1)) included in r_param etc
            end
            if length(r_param(1,:)) == 5, nump = nump + size(h_dp,2); end          % in this case no extra content parameter exists
            for jj = 1:5; nn2 = nn2 + 1; 
                [~,~,info]  = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
                matfile.metadata.par2d(:,nn2) = info';
            end

            if nump > 5
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj+1) ':E' num2str(a+jj+1)]);% info_tmp = info;
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj+1)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj+1)])) };
                for kk=1:length(h_m0(1,:)); nn2 = nn2 + 1;
                    if any(h_m0(1,kk) == [28 30 30.5 32])
                        if     strcmp(h_name2,'h_param'),        info(1)={'pm'};            info(2)={'ion mix content: [O2+,NO+]/N'};                info(5)={'pm'};  info(6)={'690'};
                        elseif strcmp(h_name2,'h_error'),        info(1)={'var_pm'};        info(2)={'variance of ion mix content: [O2+,NO+]/N'};    info(5)={'N/A'}; info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriori'),      info(1)={'aprpm'};         info(2)={'a priori ion mix content: [O2+,NO+]/N'};       info(5)={'N/A'}; info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriorierror'), info(1)={'aprpm_error'};   info(2)={'a priori error ion mix content: [O2+,NO+]/N'}; info(5)={'N/A'}; info(6)={'0'};
                        end  
                    elseif h_m0(1,kk) == 16 
                        if     strcmp(h_name2,'h_param'),        info(1)={'po'};            info(2)={'O+ content: [O+]/N'};                info(5)={'po+'};  info(6)={'620'};
                        elseif strcmp(h_name2,'h_error'),        info(1)={'var_po'};        info(2)={'variance of O+ content: [O+]/N'};    info(5)={'N/A'};  info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriori'),      info(1)={'aprpo'};         info(2)={'a priori O+ content: [O+]/N'};       info(5)={'N/A'};  info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriorierror'), info(1)={'aprpo_error'};   info(2)={'a priori error O+ content: [O+]/N'}; info(5)={'N/A'};  info(6)={'0'};
                        end
                    elseif h_m0(1,kk) == 4 
                        if     strcmp(h_name2,'h_param'),        info(1)={'phe'};           info(2)={'He+ content: [He+]/N'};                info(5)={'phe+'}; info(6)={'650'};
                        elseif strcmp(h_name2,'h_error'),        info(1)={'var_phe'};       info(2)={'variance of He+ content: [He+]/N'};    info(5)={'N/A'};  info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriori'),      info(1)={'aprphe'};        info(2)={'a priori He+ content: [He+]/N'};       info(5)={'N/A'};  info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriorierror'), info(1)={'aprphe_error'};  info(2)={'a priori error He+ content: [He+]/N'}; info(5)={'N/A'};  info(6)={'0'};
                        end
                    elseif h_m0(1,kk) == 1 
                        if     strcmp(h_name2,'h_param'),        info(1)={'ph'};            info(2)={'H+ content: [H+]/N'};                info(5)={'ph+'};  info(6)={'660'};
                        elseif strcmp(h_name2,'h_error'),        info(1)={'var_ph'};        info(2)={'variance of H+ content: [H+]/N'};    info(5)={'N/A'};  info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriori'),      info(1)={'aprph'};         info(2)={'a priori H+ content: [H+]/N'};       info(5)={'N/A'};  info(6)={'0'};
                        elseif strcmp(h_name2,'h_apriorierror'), info(1)={'aprph_error'};   info(2)={'a priori error H+ content: [H+]/N'}; info(5)={'N/A'};  info(6)={'0'};
                        end    
                    end
                    matfile.metadata.par2d(:,nn2) = info';
                end
                if length(r_param(1,:)) > 5
                    if isempty(kk); kk=0; end
                    for ll = (jj + 2):nump-(kk-2); nn2 = nn2 + 1;
                        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+ll) ':E' num2str(a+ll)]); 
                        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+ll)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+ll)]))};
                        matfile.metadata.par2d(:,nn2) = info';
                    end
                end
            end
            if strcmp(h_name2,'h_error')
                a = find(strcmp('crossvar',parameters_list)==1);
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
                if length(r_param(1,:)) == 5, nump = 5; end  % For this case ignore the extra ion composition parameters that were added (in h_param)
                for jj = 1:nump-1
                    for kk = 1:nump-jj; nn2 = nn2 + 1;
                        info(1) = {['crossvar_' num2str(kk) num2str(kk+jj)]};
                        info(2) = {['cross variance (p' num2str(kk) ',p' num2str(kk+jj) ')']};
                        matfile.metadata.par2d(:,nn2) = info';
                    end
                end
            end
        elseif strcmp(h_name2,'h_res') 
            a = find(strcmp(h_name2,parameters_list)==1);
            for jj = 1:2; nn2 = nn2+1; 
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
                info(1) = {[h_name2(3:end) num2str(jj)]};
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
                matfile.metadata.par2d(:,nn2) = info';
            end
        elseif strcmp(h_name2,'h_w') 
            a = find(strcmp(h_name2,parameters_list)==1);
            for jj = 1:3; nn2 = nn2+1; 
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
                info(1) = {[h_name2(3:end) num2str(jj)]};
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
                matfile.metadata.par2d(:,nn2) = info';
            end   
        else 
            a = find(strcmp(parameters_2d(ii),parameters_list)==1); 
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
            for jj = 1:cols, nn2 = nn2 + 1;
                info(1) = {[h_name2(3:end) num2str(jj)]};       
                matfile.metadata.par2d(:,nn2) = info';
            end
        end
    end

    nn3 = 0;
    for ii = 1:length(parameters_2dpp)
         h_name2pp = char(parameters_2dpp(ii));
         if isempty(eval(h_name2pp))
             continue
         end
         nn3 = nn3 + 1;
         a = find(strcmp(h_name2pp,parameters_list)==1);
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
         info(1) = {h_name2pp(3:end)};
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))}; 
         matfile.metadata.par2d_pp(:,nn3) = info';
    end


    % find indices where the value is complex, and replace with NaN
    if ~isempty(find(any(imag(matfile.data.par1d) ~= 0) == 1, 1))
        [rrow,ccol] = find((imag(matfile.data.par1d) ~= 0) == 1);
        for j = 1:length(rrow)
            matfile.data.par1d(rrow(j),ccol(j)) = NaN;
        end
        warning([num2str(length(rrow)) ' value(s) in matfile.metadata.par1d  was (were) complex and replaced by NaN.'])
    end
    if ~isempty(find(any(imag(matfile.data.par2d) ~= 0) == 1, 1))
        [rrow,ccol] = find((imag(matfile.data.par2d) ~= 0) == 1);
        for j = 1:length(rrow)
            matfile.data.par2d(rrow(j),ccol(j)) = NaN;
        end
        warning([num2str(length(rrow)) ' value(s) in matfile.metadata.par2d  was (were) complex and replaced by NaN.'])
    end
    if ~isempty(find(any(imag(matfile.data.par2d_pp) ~= 0) == 1, 1))
        [rrow,ccol] = find((imag(matfile.data.par2d_pp) ~= 0) == 1);
        for j = 1:length(rrow)
            matfile.data.par2d_pp(rrow(j),ccol(j)) = NaN;
        end
        warning([num2str(length(rrow)) ' value(s) in matfile.metadata.par2d_pp  was (were) complex and replaced by NaN.'])
    end


    parameters_special = {'h_om' 'h_lag' 'h_spec' 'h_freq' 'h_acf' 'h_ace'};
    for ii = 1:length(parameters_special)
        h_name = parameters_special{ii};
        name = h_name(3:end);
        if exist(['r_' name],'var') 
            a = find(strcmp(parameters_special{ii},parameters_list)==1);
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
            if isempty(info{6}), info{6} = '0'; end
            if isempty(info{7}), info{7} = '0'; end
            matfile.metadata.(name) = info';
            par = []; 
            for jj = 1:rec(rr)
                load(Filelist(jj).fname)
                par = [par;  eval(['r_' name])'];
            end

            if ~isempty(find(any(imag(par) ~= 0) == 1, 1))
                [rrow,ccol] = find((imag(par) ~= 0) == 1);      % find indices where the value is complex
                for j = 1:length(rrow)
                    par(rrow(j),ccol(j)) = NaN;
                end
                warning([num2str(length(rrow)) ' value(s) in ' matfile.metadata.(name){1} ' was complex and replaced by NaN.'])
            end
            matfile.data.(name) = par;
        end
    end

    k = length(matfile.data.par2d(1,:));
    n = length(matfile.data.par1d(1,:));

    index = [];
    pp = 0;
    for ii = 1:k
        if length(matfile.data.par2d(:,ii))==length(Filelist)
            pp = pp +1;
            matfile.data.par1d(:,n+pp)     = matfile.data.par2d(:,ii);
            matfile.metadata.par1d(:,n+pp) = matfile.metadata.par2d(:,ii);
            index = [index ii];
        end
    end
    matfile.data.par2d(:,index)     = [];
    matfile.metadata.par2d(:,index) = [];

    n = length(matfile.data.par1d(1,:));    % now possibly a new length
    index = [];
    mm = 0;
    for ii = 1:n
        if length(unique(round(matfile.data.par1d(:,ii),4)))==1 || sum(isfinite(matfile.data.par1d(:,ii))) == 0
            mm = mm + 1;
            value = matfile.data.par1d(1,ii);
            matfile.data.par0d(mm) = value;
            matfile.metadata.par0d(:,mm) = matfile.metadata.par1d(:,ii);
            index = [index ii];
        end
    end
    matfile.data.par1d(:,index)     = [];
    matfile.metadata.par1d(:,index) = [];

    if length(unique(leaps)) == 1
        ll0 = length(matfile.data.par0d);
        matfile.data.par0d(ll0+1) = leaps(1);  
        a = find(strcmp('leaps',parameters_list)==1);
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.par0d(:,ll0+1) = info';
    else
        ll1 = length(matfile.data.par1d(1,:));
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

    % space debris
    if exist('h_Sd','var')
        
        h_Sd(:,3) =  h_Sd(:,3)*1000;      % range: km --> m
        matfile.data.par0d_sd = [];
        matfile.data.par1d_sd = [];
        
        parameters_sd = {'time_sd' 'lpg_sd' 'range_sd' 'power_sd'};
        
        nn0 = 0; nn1 = 0;
        for ii = 1:length(parameters_sd)
            a = find(strcmp(parameters_sd{ii},parameters_list)==1);
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
            
            if strcmp('time_sd',parameters_sd{ii})
                [matfile.data.utime_sd,leaps_sd] = timeconv(h_Sd(:,1),'tai2unx');
                matfile.metadata.utime_sd = info';
                a = find(strcmp('leaps',parameters_list)==1);
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
                if length(unique(leaps_sd)) == 1
                    nn0 = nn0 + 1;
                    matfile.data.par0d_sd(nn0) = leaps_sd(1);  
                    matfile.metadata.par0d_sd(:,nn0) = info';
                else
                    nn1 = nn1 + 1;
                    matfile.data.par1d_sd(nn1) = leaps_sd;  
                    matfile.metadata.par1d_sd(:,nn1) = info';
                end
            else
                if length(unique(h_Sd(:,ii))) == 1
                    nn0 = nn0 + 1;
                    matfile.data.par0d_sd(:,nn0) = h_Sd(1,ii);
                    matfile.metadata.par0d_sd(:,nn0) = info';
                else
                    nn1 = nn1 + 1;
                    matfile.data.par1d_sd(:,nn1) = h_Sd(:,ii);
                    matfile.metadata.par1d_sd(:,nn1) = info';
                end
            end
        end
    end
    
    
    %%% Software
    matfile.metadata.software.software_link = {software};
    matfile.metadata.software.EISCAThdf5_ver = {hdf5ver};
    if exist('r_ver','var')
        matfile.metadata.software.GUISDAP_ver = {num2str(r_ver)};
    end
    if ~isempty(level2_link)
        matfile.metadata.level2_links = {level2_link};
    end
    if exist('name_strategy')
        matfile.metadata.software.strategy = {name_strategy};
    end

    if isfield(matfile.metadata,'utime')
        aa = find(cellfun('isempty',matfile.metadata.utime(6,:)));    matfile.metadata.utime(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.utime(7,:)));    matfile.metadata.utime(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'par0d')
        aa = find(cellfun('isempty',matfile.metadata.par0d(6,:)));    matfile.metadata.par0d(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.par0d(7,:)));    matfile.metadata.par0d(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'par1d')
        aa = find(cellfun('isempty',matfile.metadata.par1d(6,:)));    matfile.metadata.par1d(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.par1d(7,:)));    matfile.metadata.par1d(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'par2d')
        aa = find(cellfun('isempty',matfile.metadata.par2d(6,:)));    matfile.metadata.par2d(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.par2d(7,:)));    matfile.metadata.par2d(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'par2d_pp')
        aa = find(cellfun('isempty',matfile.metadata.par2d_pp(6,:))); matfile.metadata.par2d_pp(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.par2d_pp(7,:))); matfile.metadata.par2d_pp(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'utime_sd')
        aa = find(cellfun('isempty',matfile.metadata.utime_sd(6,:))); matfile.metadata.utime_sd(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.utime_sd(7,:))); matfile.metadata.utime_sd(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'par0d_sd')
        aa = find(cellfun('isempty',matfile.metadata.par0d_sd(6,:))); matfile.metadata.par0d_sd(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.par0d_sd(7,:))); matfile.metadata.par0d_sd(7,aa)= {'0'}; end
    if isfield(matfile.metadata,'par1d_sd')
        aa = find(cellfun('isempty',matfile.metadata.par1d_sd(6,:))); matfile.metadata.par1d_sd(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.par1d_sd(7,:))); matfile.metadata.par1d_sd(7,aa)= {'0'}; end
    
    symbols = ['a':'z' 'A':'Z' '0':'9'];
    strLength = 10;
    nums = randi(numel(symbols),[1 strLength]);
    randstr = symbols(nums);
    PID = ['doi://eiscat.se/3a/' year month day hour minute second '/' randstr];

    matfile.metadata.schemes.DataCite.Identifier = {PID};
    matfile.metadata.schemes.DataCite.Creator = {lower(name_ant)};
    matfile.metadata.schemes.DataCite.Title = {datafolder};
    matfile.metadata.schemes.DataCite.Publisher = {'EISCAT Scientific Association'};
    matfile.metadata.schemes.DataCite.ResourceType.Dataset = {'Level 3a Ionosphere'};
    matfile.metadata.schemes.DataCite.Date.Collected = {[starttime '/' endtime]};

    % Find the smallest box (4 corners and midpoint) to enclose the data, 
    % or 1 or unique 2 points that describes the data.
    % If area of convhull (or distance between 2 points) < 10-4 deg^2, 
    % define all points as one (average)
    % im = 1 to plot the data and the corresponding box if there is a box

    im = 0;
    [plonlat,PointInPol] = polygonpoints([gg_sp(:,2) gg_sp(:,1)],im);
    matfile.metadata.schemes.DataCite.GeoLocation.PolygonLon = plonlat(:,1);
    matfile.metadata.schemes.DataCite.GeoLocation.PolygonLat = plonlat(:,2);
    if ~isempty(PointInPol)
        matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLon = PointInPol(1);
        matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLat = PointInPol(2);
    end

    if ~isempty(gg_sp_pp)
        [plonlat,PointInPol] = polygonpoints([gg_sp_pp(:,2) gg_sp_pp(:,1)],im);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLon = plonlat(:,1);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLat = plonlat(:,2);
    end
    if ~isempty(PointInPol)
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLon = PointInPol(1);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLat = PointInPol(2);
    end

    if exist('name_sig')
        dd = strfind(name_sig,' ');
        if ~isempty(str2num(name_sig(dd(1)+1:dd(1)+2)))
            publdate = name_sig(dd(1)+1:dd(2)-1);
        else
            publdate = name_sig(dd(2)+1:dd(3)-1);
        end
        publdate = datestr(datenum(publdate,'dd-mmm-yyyy'),'yyyy-mm-dd'); % date on the form YYYY-MM-DD
        publyear = publdate(1:4);
        matfile.metadata.schemes.DataCite.PublicationYear = {publyear}; 
        matfile.metadata.schemes.DataCite.Date.Created = {publdate};
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

    % change Vi and Vobi sign
    if isfield(matfile.metadata,'par2d')
       parnames = matfile.metadata.par2d(1,:);
       a = find(strcmp('Vi',parnames));
       b = find(strcmp('Vobi',parnames));
       if a, matfile.data.par2d(:,a) = -matfile.data.par2d(:,a); end 
       if b, matfile.data.par2d(:,b) = -matfile.data.par2d(:,b); end 
    end
    if isfield(matfile.metadata,'par1d')
       parnames = matfile.metadata.par1d(1,:);
       a = find(strcmp('Vi',parnames));
       b = find(strcmp('Vobi',parnames));
       if a, matfile.data.par1d(:,a) = -matfile.data.par1d(:,a); end % change sign
       if b, matfile.data.par1d(:,b) = -matfile.data.par1d(:,b); end % change sign
    end

    % Change numeric units (1) to string ('1'), e.g. for ratios like ion compositions
    if isfield(matfile.metadata,'par0d')
        for i = 1:length(matfile.metadata.par0d(3,:))     
            if isnumeric(matfile.metadata.par0d{3,i})
                matfile.metadata.par0d(3,i) = {num2str(matfile.metadata.par0d{3,i})};
            end
        end
    end
    if isfield(matfile.metadata,'par1d')
        for i = 1:length(matfile.metadata.par1d(3,:))     
            if isnumeric(matfile.metadata.par1d{3,i})
                matfile.metadata.par1d(3,i) = {num2str(matfile.metadata.par1d{3,i})};
            end
        end
    end
    if isfield(matfile.metadata,'par2d')
        for i = 1:length(matfile.metadata.par2d(3,:))     
            if isnumeric(matfile.metadata.par2d{3,i})
                matfile.metadata.par2d(3,i) = {num2str(matfile.metadata.par2d{3,i})};
            end
        end
    end
    if isfield(matfile.metadata,'par0d_sd')
        for i = 1:length(matfile.metadata.par0d_sd(3,:))     
            if isnumeric(matfile.metadata.par0d_sd{3,i})
                matfile.metadata.par0d_sd(3,i) = {num2str(matfile.metadata.par0d_sd{3,i})};
            end
        end
    end
    if isfield(matfile.metadata,'par1d_sd')
        for i = 1:length(matfile.metadata.par1d_sd(3,:))     
            if isnumeric(matfile.metadata.par1d_sd{3,i})
                matfile.metadata.par1d_sd(3,i) = {num2str(matfile.metadata.par1d_sd{3,i})};
            end
        end
    end

    mkdir(storepath);    
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
                h5create(hdffilename,['/' char(sf) '/' char(tf)],size(matfile.(char(sf)).(char(tf))),'ChunkSize',csize,'Deflate',9,'Datatype','single');
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
                h5create(hdffilename,['/' char(sf) '/' char(tf)],size(matfile.(char(sf)).(char(tf))));
                h5write(hdffilename,['/' char(sf) '/' char(tf)],matfile.(char(sf)).(char(tf)));
            end
        end   
    end

    if addfigs
        image_filelist = [dir(fullfile(matpath,'*.png'));dir(fullfile(matpath,'*.pdf'))];
        npdf = 0;
        for ii = 1:length(image_filelist)
            figurefile = fullfile(matpath,image_filelist(ii).name);
            [~,filename,ext] = fileparts(figurefile);
            if strcmp(ext,'.png')
                store_image2Hdf5(figurefile,hdffilename)
            elseif strcmp(ext,'.pdf')
                npdf = npdf + 1;
                pdf_forHDF5(npdf) = {[filename ext]};          
            end
        end
        if npdf>0
            strds2hdf5(hdffilename,'/figures','figure_links',pdf_forHDF5');
        end
    end

    if addnotes
        notesfiles = dir(fullfile(matpath,'notes*txt'));
        for nn = 1:length(notesfiles)
            notesfile = fullfile(matpath,notesfiles(nn).name);
            addNote2Hdf5(notesfile,EISCAThdf5file{1},nn)
        end
    end
    
end
