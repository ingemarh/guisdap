% Generate a GUISDAP HDF5-file from mat-files generated in an analysis
% After analysis and calibration of an experiment an HDF5 file with all data can
% be made by calling the function mat2hdf5
% datapath points to the data (MAT-files), metadata, figures and notes from
%  the analysis and calibration of a certain experiment
% resultpath set the path where a folder containing the generated HDF5 file,
%  PDF:s of figures and a TAR file conaining all inputs is stored. The folder
%  is named the same as HDF5 file itself (but without the extension obviously).
%  For example, a file  EISCAT_yyyy_mm_dd_expr_intper@ant.hdf5 will be stored in
%  /resultpath/EISCAT_yyyy_mm_dd_expr_intper@ant/ together with PDF(s) and the TAR file.
% The inputs addfigs and addnotes  must be set non-empty (default) in order for
%  the routine to store any figures and notes*.txt in the HDF5 file.
% For multistatic experiments mat2hdf5 will call the function mat2hdf5_vel
%  to create the corresponding HDF5.
%
%function [Storepath,GUISDAPhdf5file,nvecvel,list_fortar,list_supplfortar,list_linksfortar]
%            = mat2hdf5(matpath,datapath,addfigs,addnotes,taring)

function [Storepath,GUISDAPhdf5file,nvecvel,list_fortar,list_supplfortar,list_linksfortar] = mat2hdf5(matpath,datapath,addfigs,addnotes,taring)

global path_GUP result_path name_ant hdf5ver local

name_ant = [];

if nargin<5, taring = [];  end
if nargin<4, addnotes = []; end
if nargin<3, addfigs = []; end
if isempty(taring), taring=1; end
if isempty(addnotes), addnotes=1; end
if isempty(addfigs), addfigs=1; end
if nargin==1, error('Not enough input parameters, path to matfiles folder and path to datastore folder needed'); end
if nargin<1
    matpath  = result_path;
    datapath = result_path;
end
if isstring(datapath)
    datapath = char(datapath);    % need to be char class
end

software = 'https://sourceforge.net/projects/guisdap';
GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file
[~,~,guisdappars]=xlsread(GuisdapParFile,1,['D2:D300']);
guisdappars(cellfun(@(c) isnumeric(c), guisdappars))=[];
guisdappars(cellfun(@(c) isequal(c,'N/A'), guisdappars))=[];
guisdappars=char(strjoin(string(guisdappars)));
level2_link = '';
list_fortar = {};
list_supplfortar = {};
list_linksfortar = {};

[~,text] = xlsread(GuisdapParFile);
parameters_list = text(:,1);    % list that includes all Guisdap parameters and keep their positions from the excel arc

[~,infos] = xlsread(GuisdapParFile,1,'A:E');
infos(:,6:7) = compose('%g',[NaN(3,2);xlsread(GuisdapParFile,1,'F:G')]);
[~,infodesc] = xlsread(GuisdapParFile,1,'B:B');
parameters_sd = {'time_sd' 'lpg_sd' 'range_sd' 'power_sd'};

if strcmp(matpath(end),filesep)
    matpath = matpath(1:end-1);   % remove last / in order to extract matfolder correctly
end
matpathparts = regexp(matpath,filesep,'split');
matfolder = matpathparts{end};
%[~,matfolder] = fileparts(matpath);
matpath = [matpath filesep];          % in order to make 'getfilelist.m' work

filelist_all = dir(matpath);
filelist_allmat = filelist_all(contains({filelist_all.name},'.mat'));   % consider only .mat-files
filelist = getfilelist(matpath);
if ~isempty(filelist)
    list_fortar = [list_fortar;{filelist.fname}'];
end

filelist_rest = filelist_all(~ismember({filelist_all.name},{'.','..','.gup'}) & ~contains({filelist_all.name},{'.mat','.png','.pdf','notes'})); 
for utf = 1:length(filelist_rest)
    list_supplfortar(utf,:) = {strjoin([{filelist_rest(utf).folder} {filelist_rest(utf).name}],filesep)};
end

qq = 0;
% Chech for vector velocity data
for fmata=filelist_allmat'
 hit=1;
 matfilecheck = fullfile(fmata.folder,fmata.name);
 for fmat=filelist'
  if contains(matfilecheck,fmat.fname)
   hit=0; break
  end
 end
 if hit
  if ~isempty(who('-file',matfilecheck,'Vg'))
       qq = qq + 1; 
       [storepath,file_GUISDAPhdf5] = mat2hdf5_vel(matfilecheck,datapath,addfigs,addnotes);
       GUISDAPhdf5file(qq,:) = file_GUISDAPhdf5;
       Storepath(qq,:) = storepath;
       list_fortar = [list_fortar;{matfilecheck}];
  else
       list_supplfortar = [list_supplfortar;{matfilecheck}];
  end
 end
end
nvecvel = qq;   % number of vecvel-experiments

rec  = length(filelist);
TT = []; intper_vec = []; pars_recs = []; h_sd = []; ntstamps_sd = [];
for tt = 1:rec
    clear('r_sd','r_param')
    try
        load(canon(filelist(tt).fname,0))
    catch
        continue 
    end
    TT = [TT tt];
    intper_vec_rec = posixtime(datetime(r_time(2,1:6)))-posixtime(datetime(r_time(1,1:6)));
    intper_vec = [intper_vec; intper_vec_rec];
    if exist('r_param','var')
        pars_recs = [pars_recs size(r_param,2)];
    else
        pars_recs = [pars_recs 0];
    end
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
    GUISDAPhdf5file(qq,:) = {''};
    Storepath(qq,:) = {''};
    pars_recs = [];
end

if sum(pars_recs) == 0
    pci = length(filelist);    % if no r_param exist
    params = false;
else
    % Remove isolated record(s) with another number of parameters in r_param
    % than the direct previous and direct subsequent record
    params = true;
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
    if length(pci) == 1 && any(pci == [0,1]), pci = []; end 
end

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
    ymdhms=sprintf('%04d%02d%02d%02d%02d%02.f',r_time(2,:));

    s = [];
    starttime = [];  endtime = [];
    intper_mean = []; intper_mean_str = [];
    if exist('r_gfd','var')
        gfd_Fields = fieldnames(r_gfd);
        for gf = gfd_Fields.'   
	    cgf=char(gf);
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
            elseif ~ischar(r_gfd.(cgf))
                matfile.metadata.software.gfd.(cgf) = {num2str(r_gfd.(cgf))};
            else
                matfile.metadata.software.gfd.(cgf) = {r_gfd.(cgf)};
            end
        end
        if length(pci) == 1
            starttime = datestr(r_gfd.t1,'yyyy-mm-ddTHH:MM:SS');
            endtime   = datestr(r_gfd.t2,'yyyy-mm-ddTHH:MM:SS');
            intper_mean = subsetmean(r_gfd.intper);
            intper_mean_str = num2str(round(intper_mean,3,'significant'));
        end
        % put .gup in junkfortar
        if exist(fullfile(matpath,'.gup'),'file')  
            list_supplfortar = [list_supplfortar;{fullfile(matpath,'.gup')}];
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
        list_fortar = [list_fortar;{fullfile(matpath,'.gup')}];
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

    if strcmp(matpath(end),filesep)
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
    
    if ~isempty(dd)
        ant = matfolder(dd(end)+1:end);
    else
        ant = 'eis';
    end

    if isempty(name_ant) 
       switch name_site
           case 'L', if contains('32m 42m',ant), name_ant = ant; else name_ant = 'esr'; end
           case 'K', name_ant = 'kir';
           case 'T', name_ant = 'uhf';
           case 'S', name_ant = 'sod';
           case 'V', name_ant = 'vhf';
           case 'Q', name_ant = 'quj';
           case '3', name_ant = 'san';
           case 'W', name_ant = 'wen';
           case 'D', name_ant = 'dan';
       end
    else
        name_ant = lower(name_ant);
    end

    if contains('san wen dan',name_ant)
       publisher='IGGCAS';
    else
       publisher=local.owner;
    end

    datafolder = sprintf('%s_%04d-%02d-%02d_%s_%s%s@%s',strtok(publisher),r_time(1,1:3),name_expr,name_expr_more,name_strategy ,name_ant);
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
    GUISDAPhdf5file(qq,:) = {hdffilename};

    if exist(hdffilename)==2, delete(hdffilename); end

    matfile.metadata.header= text(1,1:7)';

    % unixtime
    matfile.data.utime = [];
    leaps = [];
    ttt = 0;
    a = find(strcmp('h_time',parameters_list)==1);
    for jj = 1:2, ttt = ttt + 1;
        matfile.metadata.utime(:,ttt) = infos(a+jj,:)';
    end

    gg_sp = [];
    gg_sp_pp = [];

    [h_Magic_const, h_az, h_el, h_Pt, h_SCangle, h_XMITloc, h_RECloc, h_nrec, h_ppnrec, ...
    h_Tsys, h_code, h_om0, h_m0, h_phasepush, h_Offsetppd, h_gain, h_fradar, ...
    h_h, h_range, h_param, h_error, h_apriori, h_apriorierror, h_status, h_iter, h_dp_first, ...
    h_dp, h_ddp, h_apr, h_aprerr, h_res, h_w, h_pprange, h_pp, h_pperr, h_ppw, L2resid] = deal([]);

    unicheck_om0    = zeros(rec(rr),1);
    unicheck_gain   = zeros(rec(rr),1);
    unicheck_fradar = zeros(rec(rr),1);

    for jj = 1:rec(rr)
	eval(['clear ' guisdappars])
        load(Filelist(jj).fname)

        % UTC time
        [utime_rec,leap] = timeconv([r_time(1,:);r_time(2,:)],'utc2unx');
        matfile.data.utime = [matfile.data.utime;utime_rec'];
        leaps = [leaps;leap'];

        loc = [r_el r_az];
        if exist('r_range','var')
            h_range = [h_range; col(r_range)*1000];
            gg_rec = zeros(length(r_range),3);
            for uu = 1:length(r_range)
                gg_rec(uu,:) = loc2gg(r_RECloc,[loc r_range(uu)]);
            end
            gg_sp = [gg_sp; gg_rec];
        end
        if exist('r_pprange','var')
            h_ppnrec  = [h_ppnrec; length(r_pprange)];
            h_pprange = [h_pprange; col(r_pprange)*1000];
            gg_rec = zeros(length(r_pprange),3);
            for uu = 1:length(r_pprange)
                gg_rec(uu,:) = loc2gg(r_RECloc,[loc r_pprange(uu)]);
            end
            gg_sp_pp = [gg_sp_pp; gg_rec];
        end

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
                if size(r_param,2) == 5 && ~isempty(r_m0)     % if ion composition is missing in r_param
                    dpO = find(r_m0 == 16);
                    if isempty(dpO) || ~exist('r_dp','var')
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
                    r_gain(length(r_gain)+1:ngain) = NaN;          % if r_gain is shorter: fill it up
                elseif length(r_gain) > ngain
                    h_gain(:,ngain+1:length(r_gain)) = NaN;        % if r_gain is longer: fill up h_gain
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
	nh=0;
        if exist('r_h','var')
            nh=length(r_h);
            h_nrec = [h_nrec; nh];
            h_h = [h_h; col(r_h)*1000]; end
        if exist('r_res','var'), h_res = [h_res; r_res]; end
        if exist('r_status','var')
            if ~isempty(r_status) && length(r_status) ~= nh
                r_status = NaN(size(r_h));    % put NaN for whole record, because of wrong r_status record length
            end
            h_status = [h_status; r_status]; end
        if exist('r_iter','var')
            if ~isempty(r_iter) && length(r_iter) ~= nh
                r_iter = zeros(size(r_h));    % put 0 for whole record, because of wrong r_status record length
            end
            h_iter = [h_iter; r_iter]; end
        if exist('r_w','var')
            if size(r_w,2) == nh, r_w = r_w'; end
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
        if exist('r_pperr','var'), h_pperr = [h_pperr; r_pperr]; end
        if exist('r_pp','var'), h_pp = [h_pp; r_pp]; end
        if exist('r_ppw','var'), h_ppw = [h_ppw; r_ppw*1000]; end

        if exist('d_resid','var'), L2resid = unique([L2resid d_resid]); end
    end
    
    if params
        if size(r_param,2) == 5 && isempty(h_dp)
            display('No ion composition data available.')
        end
    end
        
    if ~any(unicheck_om0==0),    h_om0    = h_om0(:,1); end
    if ~any(unicheck_gain==0),   h_gain   = h_gain(:,1); end
    if ~any(unicheck_fradar==0), h_fradar = h_fradar(:,1); end

    if length(h_pp)    ~= length(h_pprange), h_pp    = []; end
    if length(h_pperr) ~= length(h_pprange), h_pperr = []; end
    if length(h_ppw)   ~= length(h_pprange), h_ppw   = []; end

    if exist('r_m0','var') && params
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

    nn = 0;
    if exist('name_expr','var'); nn = nn + 1; 
        infoname(1) = {'name_expr'};
        infoname(2) = {name_expr};
        a = find(strcmp('name_expr',parameters_list)==1);                
        infoname(3) = infodesc(a);
        matfile.metadata.names(:,nn) = infoname';
    end
    if exist('name_site','var'); nn = nn + 1; 
        infoname(1) = {'name_site'};
        infoname(2) = {name_site};
        a = find(strcmp('name_site',parameters_list)==1);                
        infoname(3) = infodesc(a);
        matfile.metadata.names(:,nn) = infoname';
    end
    if exist('name_ant','var'); nn = nn + 1; 
        infoname(1) = {'name_ant'};
        infoname(2) = {lower(name_ant)};
        a = find(strcmp('name_ant',parameters_list)==1); 
        infoname(3) = infodesc(a);
        matfile.metadata.names(:,nn) = infoname';
    end
    if exist('name_sig','var'); nn = nn + 1; 
        infoname(1) = {'name_sig'};
        infoname(2) = {name_sig};
        a = find(strcmp('name_sig',parameters_list)==1);                
        infoname(3) = infodesc(a);
        matfile.metadata.names(:,nn) = infoname';
    end

    matfile.data.par1d    = [h_Magic_const h_az h_el h_Pt h_SCangle h_XMITloc ...
        h_RECloc h_Tsys h_code h_om0 h_m0 h_phasepush h_Offsetppd h_gain h_fradar h_nrec h_ppnrec];
    matfile.data.par2d    = [h_h h_range h_param h_error h_apriori...
        h_apriorierror h_status h_iter h_res h_w];
    matfile.data.par2d_pp = [h_pprange h_pp h_pperr h_ppw];

    if contains(local.owner,'EISCAT') & isempty(L2resid) %try via portal
	maxtdiff=65; tdiff=find(matfile.data.utime(1,2:end)-matfile.data.utime(2,1:end-1)>maxtdiff);
	ut=[matfile.data.utime(1,[1,tdiff+1]);matfile.data.utime(2,[tdiff end])];
        tsequence=sprintf('&se=%014.3f_%014.3f',row(ut'));
	wo=weboptions; wo.Timeout=10*length(ut)+wo.Timeout;
        L2resid=str2num(webwrite('https://portal.eiscat.se/getresid',sprintf('exp_name=%s&ant=%s%s',name_expr,name_ant(1:3),tsequence),wo));
    end

    matfile.data.L2resid = L2resid';

    % 0d and 1d parameters 
    parameters_1d = {'h_Magic_const' 'h_az' 'h_el' 'h_Pt' 'h_SCangle'...
        'h_XMITloc' 'h_RECloc' 'h_Tsys' 'h_code' 'h_om0' 'h_m0' 'h_phasepush'...
        'h_Offsetppd' 'h_gain' 'h_fradar' 'h_nrec' 'h_ppnrec'};
    % 2d parameters 
    parameters_2d = {'h_h' 'h_range' 'h_param' 'h_error' 'h_apriori'...
        'h_apriorierror' 'h_status' 'h_iter' 'h_res' 'h_w'};
    % 2d_pp parameters 
    parameters_2dpp = {'h_pprange' 'h_pp' 'h_pperr' 'h_ppw'};
    % resids 
    parameters_resid = {'L2resid'};

    nn1 = 0;
    for ii = 1:length(parameters_1d)
        h_name1 = char(parameters_1d(ii));
        if isempty(eval(h_name1))
            continue
        elseif strcmp(h_name1,'h_XMITloc')
            a = find(strcmp(h_name1,parameters_list)==1);
            for jj = 1:3; nn1 = nn1+1; 
                info = {[h_name1(3:end) num2str(jj)]};
                matfile.metadata.par1d(:,nn1) = [info infos(a+jj,2:end)]';
            end
        elseif strcmp(h_name1,'h_RECloc') 
            a = find(strcmp(h_name1,parameters_list)==1);
            for jj = 1:3; nn1 = nn1+1; 
                info = {[h_name1(3:end) num2str(jj)]};
                matfile.metadata.par1d(:,nn1) = [info infos(a+jj,2:end)]';
            end
        else
            a = find(strcmp(h_name1,parameters_list)==1);             
            info = {h_name1(3:end)};
            cols = size(eval(h_name1),2);
            if cols > 1
                for jj = 1:cols; nn1 = nn1+1;
                    info = {[h_name1(3:end) num2str(jj)]};
                    matfile.metadata.par1d(:,nn1) = [info infos(a,2:end)]';
                end
            else
                nn1 = nn1+1;
                matfile.metadata.par1d(:,nn1) = [info infos(a,2:end)]';
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
            info = {h_name2(3:end)};
            matfile.metadata.par2d(:,nn2) = [info infos(a,2:end)]';
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
                matfile.metadata.par2d(:,nn2) = infos(a+jj,:)';
            end

            if nump > 5      
                for kk=1:length(h_m0(1,:)); nn2 = nn2 + 1;
                    if any(h_m0(1,kk) == [28 30 30.5 32])
                        if     strcmp(h_name2,'h_param'),        par = {'pm'};
                        elseif strcmp(h_name2,'h_error'),        par = {'var_pm'}; 
                        elseif strcmp(h_name2,'h_apriori'),      par = {'aprpm'};
                        elseif strcmp(h_name2,'h_apriorierror'), par = {'aprpm_error'};
                        end  
                    elseif h_m0(1,kk) == 16 
                        if     strcmp(h_name2,'h_param'),        par = {'po+'};
                        elseif strcmp(h_name2,'h_error'),        par = {'var_po+'};
                        elseif strcmp(h_name2,'h_apriori'),      par = {'aprpo+'};
                        elseif strcmp(h_name2,'h_apriorierror'), par = {'aprpo+_error'};
                        end
                    elseif h_m0(1,kk) == 4 
                        if     strcmp(h_name2,'h_param'),        par = {'phe+'};
                        elseif strcmp(h_name2,'h_error'),        par = {'var_phe+'};
                        elseif strcmp(h_name2,'h_apriori'),      par = {'aprphe+'};
                        elseif strcmp(h_name2,'h_apriorierror'), par = {'aprphe+_error'};
                        end
                    elseif h_m0(1,kk) == 1 
                        if     strcmp(h_name2,'h_param'),        par = {'ph+'};
                        elseif strcmp(h_name2,'h_error'),        par = {'var_ph+'};
                        elseif strcmp(h_name2,'h_apriori'),      par = {'aprph+'};
                        elseif strcmp(h_name2,'h_apriorierror'), par = {'aprph+_error'};
                        end    
                    end
                    b = find(strcmp(par,parameters_list)==1);
                    matfile.metadata.par2d(:,nn2) = infos(b,:)';
                end
                
                if length(r_param(1,:)) > 5
                    if isempty(kk); kk=0; end
                    for ll = (jj + 2):nump-(kk-2); nn2 = nn2 + 1;
                        matfile.metadata.par2d(:,nn2) = infos(a+ll,:)';
                    end
                end
            end
            if strcmp(h_name2,'h_error')
                a = find(strcmp('crossvar',parameters_list)==1);
		info=infos(a,:);
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
                info = {[h_name2(3:end) num2str(jj)]};
                matfile.metadata.par2d(:,nn2) = [info infos(a+jj,2:end)]';
            end
        elseif strcmp(h_name2,'h_w') 
            a = find(strcmp(h_name2,parameters_list)==1);
            for jj = 1:3; nn2 = nn2+1; 
                info = {[h_name2(3:end) num2str(jj)]};
                matfile.metadata.par2d(:,nn2) = [info infos(a+jj,2:end)]';
            end   
        else 
            a = find(strcmp(parameters_2d(ii),parameters_list)==1); 
            for jj = 1:cols, nn2 = nn2 + 1;
                info = {[h_name2(3:end) num2str(jj)]};
                matfile.metadata.par2d(:,nn2) = [info infos(a,2:end)]';
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
         info = {h_name2pp(3:end)};
         matfile.metadata.par2d_pp(:,nn3) = [info infos(a,2:end)]';
    end

    nn4 = 0;
    for ii = 1:length(parameters_resid)
         h_nameresid = char(parameters_resid(ii));
         if isempty(eval(h_nameresid))
             continue
         end
         nn4 = nn4 + 1;
         a = find(strcmp(h_nameresid,parameters_list)==1);
         info = {h_nameresid(3:end)};
         matfile.metadata.L2resid(:,nn4) = [info infos(a,2:end)]';
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
	    info=infos(a,:);
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

    k = size(matfile.data.par2d,2);
    n = size(matfile.data.par1d,2);

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
    if isfield(matfile.metadata,'par2d')
        matfile.metadata.par2d(:,index) = [];
    end

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
        matfile.metadata.par0d(:,ll0+1) = infos(a,:)';
    else
        ll1 = length(matfile.data.par1d(1,:));
        matfile.data.par1d(:,ll1+1:ll1+2) = leaps;
        a = find(strcmp('leaps1',parameters_list)==1);
        matfile.metadata.par1d(:,ll1+1) = infos(a,:)';
        a = find(strcmp('leaps2',parameters_list)==1);
        matfile.metadata.par1d(:,ll1+2) = infos(a,:)';
    end

    % space debris
    if exist('h_Sd','var')
        
        h_Sd(:,3) =  h_Sd(:,3)*1000;      % range: km --> m
        matfile.data.par0d_sd = [];
        matfile.data.par1d_sd = [];
        
        nn0 = 0; nn1 = 0;
        for ii = 1:length(parameters_sd)
            a = find(strcmp(parameters_sd{ii},parameters_list)==1);
            if strcmp('time_sd',parameters_sd{ii})
                [matfile.data.utime_sd,leaps_sd] = timeconv(h_Sd(:,1),'tai2unx');
                matfile.metadata.utime_sd = infos(a,:)';
                a = find(strcmp('leaps_sd',parameters_list)==1);
                if length(unique(leaps_sd)) == 1
                    nn0 = nn0 + 1;
                    matfile.data.par0d_sd(nn0) = leaps_sd(1);  
                    matfile.metadata.par0d_sd(:,nn0) = infos(a,:)';
                else
                    nn1 = nn1 + 1;
                    matfile.data.par1d_sd(nn1) = leaps_sd;  
                    matfile.metadata.par1d_sd(:,nn1) = infos(a,:)';
                end
            else
                if length(unique(h_Sd(:,ii))) == 1
                    nn0 = nn0 + 1;
                    matfile.data.par0d_sd(:,nn0) = h_Sd(1,ii);
                    matfile.metadata.par0d_sd(:,nn0) = infos(a,:)';
                else
                    nn1 = nn1 + 1;
                    matfile.data.par1d_sd(:,nn1) = h_Sd(:,ii);
                    matfile.metadata.par1d_sd(:,nn1) = infos(a,:)';
                end
            end
        end
    end
    
    
    %%% Software
    matfile.metadata.software.software_link = {software};
    matfile.metadata.software.GUISDAPhdf5_ver = {hdf5ver};
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
    if isfield(matfile.metadata,'L2resid')
        aa = find(cellfun('isempty',matfile.metadata.L2resid(6,:))); matfile.metadata.L2resid(6,aa)= {'0'};
        aa = find(cellfun('isempty',matfile.metadata.L2resid(7,:))); matfile.metadata.L2resid(7,aa)= {'0'}; end
    
    symbols = ['a':'z' 'A':'Z' '0':'9'];
    strLength = 10;
    nums = randi(numel(symbols),[1 strLength]);
    randstr = symbols(nums);
    PID = ['doi://eiscat.se/3a/' ymdhms '/' randstr];

    matfile.metadata.schemes.DataCite.Identifier = {PID};
    matfile.metadata.schemes.DataCite.Creator = {lower(name_ant)};
    matfile.metadata.schemes.DataCite.Title = {datafolder};
    matfile.metadata.schemes.DataCite.Publisher = {publisher};
    matfile.metadata.schemes.DataCite.ResourceType.Dataset = {'Level 3a Ionosphere'};
    matfile.metadata.schemes.DataCite.Date.Collected = {[starttime '/' endtime]};

    % Find the smallest box (4 corners and midpoint) to enclose the data, 
    % or 1 or unique 2 points that describes the data.
    % If area of convhull (or distance between 2 points) < 10-4 deg^2, 
    % define all points as one (average)
    % im = 1 to plot the data and the corresponding box if there is a box

    im = 0;
    if ~isempty(gg_sp)
        [plonlat,PointInPol] = polygonpoints([gg_sp(:,2) gg_sp(:,1)],im);
        matfile.metadata.schemes.DataCite.GeoLocation.PolygonLon = plonlat(:,1);
        matfile.metadata.schemes.DataCite.GeoLocation.PolygonLat = plonlat(:,2);
    end    
    if exist('PointInPol','var') && ~isempty(PointInPol)
        matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLon = PointInPol(1);
        matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLat = PointInPol(2);
    end

    if ~isempty(gg_sp_pp)
        [plonlat_pp,PointInPol_pp] = polygonpoints([gg_sp_pp(:,2) gg_sp_pp(:,1)],im);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLon = plonlat_pp(:,1);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLat = plonlat_pp(:,2);
    end
    if exist('PointInPol_pp','var') && ~isempty(PointInPol_pp)
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLon = PointInPol_pp(1);
        matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLat = PointInPol_pp(2);
    end

    if exist('name_sig')
        months = {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'};
        % assuming date in name_sig is always in the form DD-mmm-YYYY, (e.g. 06-Feb-1980)
        mon = find(contains(months,strsplit(name_sig,'-')));  
        dd = strfind(name_sig,months(mon));
        publdate = name_sig(dd-3:dd+7);
        publdate = datestr(datenum(publdate,'dd-mmm-yyyy'),'yyyy-mm-dd'); % date on the form YYYY-MM-DD
        publyear = publdate(1:4);
        matfile.metadata.schemes.DataCite.PublicationYear = {publyear}; 
        matfile.metadata.schemes.DataCite.Date.Created = {publdate};
    end

    % Delete any empty fields from the structure
    sFields = fieldnames(matfile);
    for sf = sFields.' 
	csf=char(sf);
        tFields = fieldnames(matfile.(csf));
        for tf = tFields.'
	    ctf=char(tf);
            if isempty(matfile.(csf).(ctf))
                matfile.(csf) = rmfield(matfile.(csf),(ctf));
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
    % save(matfilename,'matfile')

    % Generate an HDF5-file from the MAT-file
    if exist(hdffilename)
     hdfid = H5F.open(hdffilename,'H5F_ACC_RDWR','H5P_DEFAULT');
    else
     hdfid = H5F.create(hdffilename);
    end
    sFields = fieldnames(matfile);
    for sf = sFields.'
        csf=char(sf);
        group1 = ['/' csf];
        tFields = fieldnames(matfile.(csf));
        for tf = tFields.'
            ctf=char(tf);
            if strcmp('data',csf) && any(strcmp({'par0d' 'par1d' 'par2d' 'par2d_pp' 'acf' 'ace' 'lag' 'freq' 'spec' 'om'},ctf))
                strds2hdf5(hdfid,['/' csf],ctf,single(matfile.(csf).(ctf)))
            elseif strcmp('metadata',csf) 
                if isstruct(matfile.(csf).(ctf))
                    group2 = [group1 '/' ctf];
                    uFields = fieldnames(matfile.(csf).(ctf));
                    for uf = uFields.'
                        cuf=char(uf);
                        if isstruct(matfile.(csf).(ctf).(cuf))
                            group3 = [group2 '/' cuf];
                            vFields = fieldnames(matfile.(csf).(ctf).(cuf));
                            for vf = vFields.'
                                cvf=char(vf);
                                if isstruct(matfile.(csf).(ctf).(cuf).(cvf))
                                    group4 = [group3 '/' cvf];
                                    wFields = fieldnames(matfile.(csf).(ctf).(cuf).(cvf));
                                    for wf = wFields.'
                                        cwf=char(wf);
                                        strdata = matfile.(csf).(ctf).(cuf).(cvf).(cwf);
                                        dsname = cwf;
                                        strds2hdf5(hdfid,group4,dsname,strdata)
                                    end
                                else
                                    strdata = matfile.(csf).(ctf).(cuf).(cvf);
                                    dsname = cvf;
                                    strds2hdf5(hdfid,group3,dsname,strdata)
                                end
                            end
                        else
                            strdata = matfile.(csf).(ctf).(cuf);
                            dsname = cuf;
                            strds2hdf5(hdfid,group2,dsname,strdata)
                        end
                    end
                else
                    strdata = matfile.(csf).(ctf);
                    dsname = ctf;
                    strds2hdf5(hdfid,group1,dsname,strdata)
                end
            else
                strds2hdf5(hdfid,['/' csf],ctf,matfile.(csf).(ctf))
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
                image2hdf5(figurefile,hdfid)
                list_fortar = [list_fortar;{figurefile}];
            elseif strcmp(ext,'.pdf')
                npdf = npdf + 1;
                pdf_forHDF5(npdf) = {[filename ext]};  
                list_linksfortar = [list_linksfortar;{figurefile}];
            end
        end
        if npdf>0
            strds2hdf5(hdfid,'/figures','figure_links',pdf_forHDF5');
        end
    end

    if addnotes
        notesfiles = dir(fullfile(matpath,'notes*txt'));
        for nn = 1:length(notesfiles)
            notesfile = fullfile(matpath,notesfiles(nn).name);
            note2hdf5(notesfile,hdfid,nn)
            list_fortar = [list_fortar;{notesfile}];
        end
    end
    close(hdfid)
end
    
if taring
 reltar(fullfile(storepath,[datafolder '.tar.gz']),list_fortar);
 reltar(fullfile(storepath,[datafolder '_linked.tar.gz']),list_linksfortar);
 reltar(fullfile(storepath,[datafolder '_suppl.tar.gz']),list_supplfortar); 
end

function reltar(out,list)
if ~isempty(list)
 newlist=cell(size(list));
 i=0;
 for l=list'
  [root,nl,ne]=fileparts(char(l));
  [rp,d,e]=fileparts(root);
  i=i+1;
  newlist{i}=fullfile([d e],[nl ne]);
 end
 [op,d,e]=fileparts(out);
 fullout=fullfile(what(op).path,[d e]);
 owd=pwd;
 if ~isempty(rp), cd(rp), end
 try
  tar(fullout,newlist)
 catch, end
 cd(owd)
end
