% Generate an EISCAT HDF5-file from mat-files generated in a Guisdap analysis

function [storepath,EISCAThdf5file] = mat2hdf5(matpath, datapath,addfigs,addnotes) 

global path_GUP result_path name_ant

name_ant = [];

if nargin<4, addnotes = []; else addnotes = 1; end 
if nargin<3, addfigs = []; else addfigs = 1; end 
if nargin==1, error('Not enough input parameters, path to matfiles folder and path to datastore folder needed'); end
if nargin<1
    matpath = result_path;
    datapath = result_path;
end
if isstring(datapath)
    datapath = char(datapath);    % need to be char class
end

filelist   = dir(fullfile(matpath,'*.mat')); 
rec        = length(filelist);

gupfile    = fullfile(matpath,'.gup');
gupfilecheck =  dir(gupfile);

intper_vec = zeros(rec,1);
for tt = 1:rec
    %matfile_tmp = fullfile(matpath,filelist(tt).name);
    load(fullfile(matpath,filelist(tt).name))
    intper_vec(tt) = posixtime(datetime(r_time(2,1:6)))-posixtime(datetime(r_time(1,1:6))); 
end
if median(intper_vec) < 10
    intper_med = median(intper_vec);
    intper_med_str = sprintf('%.2f',intper_med);
end

% store the gfd content
load( fullfile(matpath,filelist(1).name));
s = [];
if exist('r_gfd','var')
    %matfile.metadata.gfd = r_gfd;
    gfd_Fields = fieldnames(r_gfd);
    for gf = gfd_Fields.'   
        if strcmp(gf,'extra')
            extra=r_gfd.extra;
            for r = 1:size(extra,1)
                if contains(extra(r,:),'%')
                    s = [s r];
                end
            end
            extra(s,:)=[];
            if ~isempty(extra)
                matfile.metadata.gfd.extra = {row([extra ones(size(extra,1))*'#']')};
            else
                matfile.metadata.gfd.extra = {'None'};
            end
        elseif ~ischar(r_gfd.(char(gf)))
            matfile.metadata.gfd.(char(gf)) = {num2str(r_gfd.(char(gf)))};
        else
            matfile.metadata.gfd.(char(gf)) = {r_gfd.(char(gf))};
        end
    end
    starttime = datestr(r_gfd.t1,'yyyy-mm-ddTHH:MM:SS');
    endtime   = datestr(r_gfd.t2,'yyyy-mm-ddTHH:MM:SS');
    if ~exist('intper_med','var')
        intper_med = median(r_gfd.intper);
        intper_med_str = num2str(intper_med);
    end
    matfile.metadata.gfd.intper_median = {num2str(intper_med)};
elseif ~isempty(gupfilecheck)
    load('-mat',gupfile);
    if exist('name_expr','var'),    matfile.metadata.gfd.name_expr      = {name_expr};           end
    if exist('expver','var'),       matfile.metadata.gfd.expver         = {num2str(expver)};     end
    if exist('siteid','var'),       matfile.metadata.gfd.siteid         = {num2str(siteid)};     end
    if exist('data_path','var'),    matfile.metadata.gfd.data_path      = {data_path};           end
    if exist('result_path','var'),  matfile.metadata.gfd.result_path    = {result_path};         end 
    if exist('intper','var'),       matfile.metadata.gfd.intper         = {num2str(intper)};  
        if ~exist('intper_med','var'), intper_med = median(intper); intper_med_str = num2str(intper_med); end
                                    matfile.metadata.gfd.intper_median  = {num2str(intper_med)}; end
    if exist('t1','var'),           matfile.metadata.gfd.t1             = {num2str(t1)};         end
    if exist('t2','var'),           matfile.metadata.gfd.t2             = {num2str(t2)};         end
    if exist('rt','var'),           matfile.metadata.gfd.rt             = {num2str(rt)};         end
    if exist('figs','var'),         matfile.metadata.gfd.figs           = {num2str(figs)};       end
    if exist('path_exps','var'),    matfile.metadata.gfd.path_exps      = {path_exps};           end
    if exist('extra','var')
        for r = 1:size(extra,1)
            if contains(extra(r,:),'%')
                s = [s r];
            end
        end
        extra(s,:)=[];
        if ~isempty(extra)
            matfile.metadata.gfd.extra = {row([extra ones(size(extra,1))*'#']')};
        else
            matfile.metadata.gfd.extra = {'None'};
        end
        
    end
    starttime = datestr(t1,'yyyy-mm-ddTHH:MM:SS');
    endtime   = datestr(t2,'yyyy-mm-ddTHH:MM:SS');
end

if ~exist('starttime','var')
    matfile_tmp = fullfile(matpath,filelist(1).name);
    load(matfile_tmp)
    starttime = datestr(r_time(1,:),'yyyy-mm-ddTHH:MM:SS');
    matfile_tmp = fullfile(matpath,filelist(1).name);
    load(matfile_tmp)
    endtime = datestr(r_time(2,:),'yyyy-mm-ddTHH:MM:SS');
end
    

gg_sp    = [];
gg_sp_pp = [];
for tt = 1:rec
    matfile_tmp = fullfile(matpath,filelist(tt).name);
    load(matfile_tmp)
    loc(1:2) = [r_el r_az];
    gg_rec = [];
    for uu = 1:length(r_range)
        loc(3) = r_range(uu);
        gg_point = loc2gg(r_RECloc,loc);
        gg_rec = [gg_rec; gg_point];
    end
    gg_sp = [gg_sp; gg_rec];
    gg_rec = [];
    for uu = 1:length(r_pprange)
        loc(3) = r_pprange(uu);
        gg_point = loc2gg(r_RECloc,loc);
        gg_rec = [gg_rec; gg_point];
    end
    gg_sp_pp = [gg_sp_pp; gg_rec];
end
%ndatapoints = length(gg_sp(:,1));

software = 'https://git.eiscat.se/cvs/guisdap9';
level2_link = '';
matfile.metadata.software_link = {software};
if ~isempty(level2_link)
    matfile.metadata.level2_links = {level2_link};
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
[~,matfolder] = fileparts(matpath);
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
       case 'L', if isempty(str2num(ant)), name_ant = ant; else name_ant = 'esr'; end
       case 'K', name_ant = 'kir';
       case 'T', name_ant = 'uhf';
       case 'S', name_ant = 'sod';
       case 'V', name_ant = 'vhf';
       case 'Q', name_ant = 'quj';
   end
end

datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_' name_expr_more intper_med_str '@' name_ant];
%display(datafolder)

storepath = fullfile(datapath,datafolder);
if exist(storepath)
   rmdir(storepath,'s');
end
mkdir(storepath);

Hdf5File = sprintf('%s%s',datafolder,'.hdf5');
MatFile = sprintf('%s%s',datafolder,'.mat');
hdffilename = fullfile(storepath,Hdf5File);
matfilename = fullfile(storepath,MatFile);
EISCAThdf5file = hdffilename;
GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file

if exist(hdffilename)==2, delete(hdffilename); end

% 0d and 1d parameters 
parameters_1d = {'h_Magic_const' 'h_az' 'h_el' 'h_Pt' 'h_SCangle'...
    'h_XMITloc' 'h_RECloc' 'h_Tsys' 'h_code' 'h_om0' 'h_m0' 'h_phasepush'...
    'h_Offsetppd' 'h_gain' 'h_fradar'};
% 2d parameters 
parameters_2d = {'h_h' 'h_range' 'h_param' 'h_error' 'h_apriori'...
    'h_apriorierror' 'h_status' 'h_dp' 'h_res' 'h_w'};
% 2d_pp parameters 
parameters_2dpp = {'h_pprange' 'h_pp' 'h_pperr' 'h_ppw'};

[~,text] = xlsread(GuisdapParFile);
parameters_list = text(:,1);    % list that includes all Guisdap parameters and keep their positions from the excel arc

matfile.metadata.header= text(1,1:7)';

% unixtime
timepar = [];
ttt = 0;
a = find(strcmp('h_time',parameters_list)==1);
for jj = 1:2; ttt = ttt+1;
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
    info(1) = {['time' num2str(jj)]};
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
    matfile.metadata.utime(:,ttt) = info';
end
for jj = 1:rec
     matfile_tmp = fullfile(matpath,filelist(jj).name);
     load(matfile_tmp)
     timepar = [timepar; posixtime(datetime(r_time(1,:))) posixtime(datetime(r_time(2,:)))];   % unix time
end
matfile.data.utime = timepar;
            

nn1 = 0;
[uniom0,unigain,unifradar] = deal(0);
for ii = 1:length(parameters_1d)
     h_name1 = char(parameters_1d(ii));
     if ~exist(['r_' h_name1(3:end)],'var')
         continue
%      elseif strcmp(h_name1,'h_time')
%          a = find(strcmp(h_name1,parameters_list)==1);
%          for jj = 1:2; nn1 = nn1+1;
%             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
%             info(1) = {[h_name1(3:end) num2str(jj)]};
%             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
%             matfile.metadata.par1d(:,nn1) = info';
%          end
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
     elseif strcmp(h_name1,'h_om0') && length(unique(eval(['r_om0']))) == 1
         nn1 = nn1+1;
         uniom0 = 1;
         a = find(strcmp(h_name1,parameters_list)==1);
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
         info(1) = {[h_name1(3:end)]};
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
         matfile.metadata.par1d(:,nn1) = info';
     elseif strcmp(h_name1,'h_gain') && length(unique(eval(['r_gain']))) == 1
         nn1 = nn1+1;
         unigain = 1;
         a = find(strcmp(h_name1,parameters_list)==1);
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
         info(1) = {[h_name1(3:end)]};
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
         matfile.metadata.par1d(:,nn1) = info';   
     elseif strcmp(h_name1,'h_fradar') && length(unique(eval(['r_fradar']))) == 1
         nn1 = nn1+1;
         unifradar = 1;
         a = find(strcmp(h_name1,parameters_list)==1);
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
         info(1) = {[h_name1(3:end)]};
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
         matfile.metadata.par1d(:,nn1) = info';    
     elseif length(eval(['r_' h_name1(3:end)]))==1
         nn1 = nn1+1;
         a = find(strcmp(h_name1,parameters_list)==1);             
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
         info(1) = {h_name1(3:end)};
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))}; 
         matfile.metadata.par1d(:,nn1) = info';
     else
         num = length(eval(['r_' h_name1(3:end)]));
         a = find(strcmp(h_name1,parameters_list)==1);
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
         for jj = 1:num; nn1 = nn1+1;
             info(1) = {[h_name1(3:end) num2str(jj)]};  
             matfile.metadata.par1d(:,nn1) = info';
         end
     end
     
end

% adding record lengths 
nn1 = nn1 + 1;
a = find(strcmp('nrec',parameters_list)==1);
[~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
matfile.metadata.par1d(:,nn1) = info';

nn1 = nn1 + 1;
a = find(strcmp('nrec_pp',parameters_list)==1);
[~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
matfile.metadata.par1d(:,nn1) = info';

nn2 = 0;
for ii = 1:length(parameters_2d)
    h_name2 = char(parameters_2d(ii));
    if ~exist(['r_' h_name2(3:end)],'var')
        continue
    end
    dim=size(eval(['r_' h_name2(3:end)]));
    if dim(2)==1
        nn2 = nn2 + 1;
        a = find(strcmp(h_name2,parameters_list)==1);
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
        info(1) = {h_name2(3:end)};
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
        matfile.metadata.par2d(:,nn2) = info';
    elseif strcmp(h_name2,'h_param') || strcmp(h_name2,'h_error') || strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror')
        a = find(strcmp(h_name2,parameters_list)==1);
        if strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror')
            nump = length(r_apriori(1,:));
        else
            nump = length(r_param(1,:));
        end
        for jj = 1:5; nn2 = nn2 + 1; 
            [~,~,info]  = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
            matfile.metadata.par2d(:,nn2) = info';
        end
        [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj+1) ':E' num2str(a+jj+1)]); info_tmp = info;
        info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj+1)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj+1)])) };
        if ge(nump,6)
            for kk=1:length(r_m0)-1; nn2 = nn2 + 1; 
                if  (length(r_m0)-1)>1
                    info(1) = {[char(info_tmp(1)) num2str(kk)]};
                end
                if r_m0(kk)==30.5   
                    if     strcmp(h_name2,'h_param'),        info(2)={'ion mix content: [O2+,NO+]/N'};                info(5)={'pm'};  info(6)={'690'};
                    elseif strcmp(h_name2,'h_error'),        info(2)={'error ion mix content: [O2+,NO+]/N'};          info(5)={'dpm'}; info(6)={'-690'};
                    elseif strcmp(h_name2,'h_apriori'),      info(2)={'a priori ion mix content: [O2+,NO+]/N'};       info(5)={'N/A'}; info(6)={'0'};
                    elseif strcmp(h_name2,'h_apriorierror'), info(2)={'a priori error ion mix content: [O2+,NO+]/N'}; info(5)={'N/A'}; info(6)={'0'};
                    end  
                elseif r_m0(kk)==16 
                    if     strcmp(h_name2,'h_param'),        info(2)={'O+ content: [O+]/N'};                info(5)={'po+'};  info(6)={'620'};
                    elseif strcmp(h_name2,'h_error'),        info(2)={'error O+ content: [O+]/N'};          info(5)={'dpo+'}; info(6)={'-620'};
                    elseif strcmp(h_name2,'h_apriori'),      info(2)={'a priori O+ content: [O+]/N'};       info(5)={'N/A'};  info(6)={'0'};
                    elseif strcmp(h_name2,'h_apriorierror'), info(2)={'a priori error O+ content: [O+]/N'}; info(5)={'N/A'};  info(6)={'0'};
                    end
                end
                matfile.metadata.par2d(:,nn2) = info';
            end
            if isempty(kk); kk=0; end
            for ll = (jj + 2):nump; nn2 = nn2 + 1;
                [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+ll) ':E' num2str(a+ll)]); 
                info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+ll)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+ll)]))};
                matfile.metadata.par2d(:,nn2) = info';
            end
        end
        if strcmp(h_name2,'h_error')
            a = find(strcmp('crossvar',parameters_list)==1);
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
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
        for jj = 1:dim(2); nn2 = nn2 + 1;
            info(1) = {[h_name2(3:end) num2str(jj)]};       
            matfile.metadata.par2d(:,nn2) = info';
        end
    end
end

nn3 = 0;
for ii = 1:length(parameters_2dpp)
     h_name2pp = char(parameters_2dpp(ii));
     if ~exist(['r_' h_name2pp(3:end)],'var')
         continue
     end
     nn3 = nn3 + 1;
     a = find(strcmp(h_name2pp,parameters_list)==1);
     [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
     info(1) = {h_name2pp(3:end)};
     info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))}; 
     matfile.metadata.par2d_pp(:,nn3) = info';
end

parameters = [parameters_1d parameters_2d parameters_2dpp];

nh = [];
npprange = [];

for ii = 1:length(parameters)
    h_name = char(parameters(ii));
    if ~exist(['r_' h_name(3:end)],'var') 
        evalc([h_name '=[]']); continue
    else
        par = []; 
        for jj = 1:rec
            matfile_tmp = fullfile(matpath,filelist(jj).name);
            load(matfile_tmp)
%             if strcmp(h_name,'h_time')
%                 par = [par; posixtime(datetime(r_time(1,:))) posixtime(datetime(r_time(2,:)))];   % unix time
            if strcmp(h_name,'h_Tsys')
                par = [par; eval(['r_' h_name(3:end)])'];                                                
            elseif strcmp(h_name,'h_h')
                nh = [nh; length(r_h)];
                par = [par; eval(['r_' h_name(3:end)])];
            elseif strcmp(h_name,'h_pprange')
                npprange = [npprange; length(r_pprange)];
                par = [par; eval(['r_' h_name(3:end)])];
            elseif strcmp(h_name,'h_om0') && uniom0 == 1
                r_om0 = r_om0(1);  
                par = [par; eval(['r_' h_name(3:end)])]; 
            elseif strcmp(h_name,'h_gain') && unigain == 1
                r_gain = r_gain(1);
                par = [par; eval(['r_' h_name(3:end)])]; 
            elseif strcmp(h_name,'h_fradar') && unifradar == 1
                r_fradar = r_fradar(1);
                par = [par; eval(['r_' h_name(3:end)])];    
            else
                par = [par; eval(['r_' h_name(3:end)])];    
            end
        end       
        evalc([char(parameters(ii)) '=par']);
    end
end

matfile.data.par1d    = [h_Magic_const h_az h_el h_Pt...
    h_SCangle h_XMITloc h_RECloc h_Tsys h_code h_om0 h_m0...
    h_phasepush h_Offsetppd h_gain h_fradar nh npprange];

matfile.data.par2d    = [h_h h_range h_param h_error h_apriori...
    h_apriorierror h_status h_dp h_res h_w];

matfile.data.par2d_pp = [h_pprange h_pp h_pperr h_ppw];


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
        for jj = 1:rec
            matfile_tmp = fullfile(matpath,filesep,filelist(jj).name);
            load(matfile_tmp)
            par = [par;  eval(['r_' name])'];
            end      
        matfile.data.(name) = par;
    end
end

k = length(matfile.data.par2d(1,:));
n = length(matfile.data.par1d(1,:));

index = [];
pp = 0;
for ii = 1:k
    if length(matfile.data.par2d(:,ii))==length(filelist)
        pp = pp +1;
        matfile.data.par1d(:,n+pp)     = matfile.data.par2d(:,ii);
        matfile.metadata.par1d(:,n+pp) = matfile.metadata.par2d(:,ii);
        index = [index ii];
    end
end
matfile.data.par2d(:,index)     = [];
matfile.metadata.par2d(:,index) = [];

n = length(matfile.data.par1d(1,:));    % now possibly a new length
mm=0;
index = [];

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
    infoname(2) = {name_ant};
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

if exist('r_ver','var'); nn = nn + 1; 
    infoname(1) = {'gupver'};
    infoname(2) = {num2str(r_ver)};
    a = find(strcmp('h_ver',parameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end

aa = find(cellfun('isempty',matfile.metadata.par0d(6,:)));    matfile.metadata.par0d(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par0d(7,:)));    matfile.metadata.par0d(7,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par1d(6,:)));    matfile.metadata.par1d(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par1d(7,:)));    matfile.metadata.par1d(7,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d(6,:)));    matfile.metadata.par2d(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d(7,:)));    matfile.metadata.par2d(7,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d_pp(6,:))); matfile.metadata.par2d_pp(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d_pp(7,:))); matfile.metadata.par2d_pp(7,aa)= {'0'};
  
symbols = ['a':'z' 'A':'Z' '0':'9'];
strLength = 10;
nums = randi(numel(symbols),[1 strLength]);
randstr = symbols(nums);
PID = ['doi://eiscat.se/3a/' year month day hour minute second '/' randstr];

matfile.metadata.schemes.DataCite.Identifier = {PID};
matfile.metadata.schemes.DataCite.Creator = {name_ant};
matfile.metadata.schemes.DataCite.Title = {datafolder};
matfile.metadata.schemes.DataCite.Publisher = {'EISCAT Scientific Association'};
matfile.metadata.schemes.DataCite.ResourceType.Dataset = {'Level 3 Ionosphere'};
matfile.metadata.schemes.DataCite.Date.Collected = {[starttime '/' endtime]};

% Find the smallest box (4 corners and mid-point) to enclose the data, 
% or 1 or unique 2 points that describes the data.
% If area of convhull (or distance between 2 points) < 10-4 deg^2, 
% define all points as one (average)
% imag = 1 to plot the data and the corresponding box if there is a box
im = 1;

[plonlat,PointInPol] = polygonpoints([gg_sp(:,2) gg_sp(:,1)],im);
matfile.metadata.schemes.DataCite.GeoLocation.PolygonLon = plonlat(:,1);
matfile.metadata.schemes.DataCite.GeoLocation.PolygonLat = plonlat(:,2);
if ~isempty(PointInPol)
    matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLon = PointInPol(1);
    matfile.metadata.schemes.DataCite.GeoLocation.PointInPolygonLat = PointInPol(2);
end

[plonlat,PointInPol] = polygonpoints([gg_sp_pp(:,2) gg_sp_pp(:,1)],im);
matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLon = plonlat(:,1);
matfile.metadata.schemes.DataCite.GeoLocation_pp.PolygonLat = plonlat(:,2);
if ~isempty(PointInPol)
    matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLon = PointInPol(1);
    matfile.metadata.schemes.DataCite.GeoLocation_pp.PointInPolygonLat = PointInPol(2);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

if addfigs
    image_filelist = [dir(fullfile(matpath,'*.png'));dir(fullfile(matpath,'*.pdf'))];
    npdf = 0;
    %if ~isempty(image_filelist)
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
        strds2hdf5(hdffilename,'/metadata','figure_links',pdf_forHDF5');
      end
    %end
end

if addnotes
    notesfiles = dir(fullfile(matpath,'notes*txt'));
    for nn = 1:length(notesfiles)
        notesfile = fullfile(matpath,notesfiles(nn).name);
        addNote2Hdf5(notesfile,EISCAThdf5file,nn)
    end
end

end

