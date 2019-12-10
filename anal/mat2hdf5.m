% Generate an EISCAT HDF5-file from mat-files generated in a Guisdap analysis

function [storepath,EISCAThdf5file] = mat2hdf5(matpath, datapath)
 
%global newhdf5file
global path_GUP result_path name_ant
name_ant = [];
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

matfile_1 = fullfile(matpath,filelist(1).name); 
load(matfile_1);

% store the gfd content
s = [];
if exist('r_gfd','var')
    matfile.metadata.gfd = r_gfd;
    extra=r_gfd.extra;
    for r = 1:size(extra,1)
        if contains(extra(r,:),'%')
            s = [s r];
        end
    end
    extra(s,:)=[];
    matfile.metadata.gfd.extra = row([extra ones(size(extra,1))*'#']');
    intper_med = median(r_gfd.intper);
    matfile.metadata.gfd.intper_median = intper_med;
elseif ~isempty(gupfilecheck)
    load('-mat',gupfile);
    if exist('name_expr','var'),    matfile.metadata.gfd.name_expr      = name_expr;   end
    if exist('expver','var'),       matfile.metadata.gfd.expver         = expver;      end
    if exist('siteid','var'),       matfile.metadata.gfd.siteid         = siteid;      end
    if exist('data_path','var'),    matfile.metadata.gfd.data_path      = data_path;   end
    if exist('result_path','var'),  matfile.metadata.gfd.result_path    = result_path; end 
    if exist('intper','var'),       matfile.metadata.gfd.intper         = intper;  
       intper_med = median(intper); matfile.metadata.gfd.intper_median  = intper_med;  end
    if exist('t1','var'),           matfile.metadata.gfd.t1             = t1;          end
    if exist('t2','var'),           matfile.metadata.gfd.t2             = t2;          end
    if exist('rt','var'),           matfile.metadata.gfd.rt             = rt;          end
    if exist('figs','var'),         matfile.metadata.gfd.figs           = figs;        end
    if exist('path_exps','var'),    matfile.metadata.gfd.path_exps      = path_exps;   end
    if exist('extra','var')
        for r = 1:size(extra,1)
            if contains(extra(r,:),'%')
                s = [s r];
            end
        end
        extra(s,:)=[];
        matfile.metadata.gfd.extra       = row([extra ones(size(extra,1))*'#']'); 
    end
else 
    intper_vec = zeros(rec,1);
    for tt = 1:rec
        matfile_tmp = fullfile(matpath,filelist(tt).name);
        load(matfile_tmp)
        intper_vec(tt) = posixtime(datetime(r_time(2,1:6)))-posixtime(datetime(r_time(1,1:6)));
        intper_med = median(intper_vec);
    end
end
software = 'https://git.eiscat.se/cvs/guisdap9';
level2_link = [];
matfile.metadata.software_link = software;
matfile.metadata.level2_links = level2_link;

year = num2str(r_time(1,1));
month = sprintf('%02d',r_time(1,2));
day = sprintf('%02d',r_time(1,3));

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

datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_' name_expr_more num2str(intper_med) '@' name_ant];
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
parameters_1d = {'h_time' 'h_ver' 'h_Magic_const' 'h_az' 'h_el' 'h_Pt' 'h_SCangle'...
    'h_XMITloc' 'h_RECloc' 'h_Tsys' 'h_code' 'h_om0' 'h_m0' 'h_phasepush'...
    'h_Offsetppd'};
% 2d parameters 
parameters_2d = {'h_h' 'h_range' 'h_param' 'h_error' 'h_apriori'...
    'h_apriorierror' 'h_status' 'h_dp' 'h_res' 'h_w'};
% 2d_pp parameters 
parameters_2dpp = {'h_pprange' 'h_pp' 'h_pperr' 'h_ppw'};

[~,text] = xlsread(GuisdapParFile);
parameters_list = text(:,1);    % list that includes all Guisdap parameters and keep their positions from the excel arc

matfile.metadata.header= text(1,1:7)';

nn1 = 0;
for ii = 1:length(parameters_1d)
     h_name1 = char(parameters_1d(ii));
     if ~exist(['r_' h_name1(3:end)],'var')
         continue
     elseif strcmp(h_name1,'h_time')
         a = find(strcmp(h_name1,parameters_list)==1);
         for jj = 1:2; nn1 = nn1+1;
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
            info(1) = {[h_name1(3:end) num2str(jj)]};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
            matfile.metadata.par1d(:,nn1) = info';
         end
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
     elseif strcmp(h_name2,'h_param') || strcmp(h_name2,'h_error') || strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror' )
         nump = length(r_param(1,:));
         a = find(strcmp(h_name2,parameters_list)==1);
         for jj = 1:5; nn2 = nn2 + 1; 
            [~,~,info]  = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
            matfile.metadata.par2d(:,nn2) = info';
         end
         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj+1) ':E' num2str(a+jj+1)]); info_tmp = info;
         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj+1)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj+1)])) };
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
         if strcmp(h_name2,'h_error')
             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+nump+1) ':E' num2str(a+nump+1)]);
             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+nump+1)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+nump+1)]))};
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
parameters_special = {'h_om' 'h_lag' 'h_spec' 'h_freq' 'h_acf' 'h_ace'};

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
            if strcmp(h_name,'h_time')
                par = [par; posixtime(datetime(r_time(1,:))) posixtime(datetime(r_time(2,:)))];   % unix time
            elseif strcmp(h_name,'h_Tsys')
                par = [par; eval(['r_' h_name(3:end)])'];                                                
            elseif strcmp(h_name,'h_h')
                nh = [nh; length(r_h)];
                par = [par; eval(['r_' h_name(3:end)])];
            elseif strcmp(h_name,'h_pprange')
                npprange = [npprange; length(r_pprange)];
                par = [par; eval(['r_' h_name(3:end)])];
            else
                par = [par; eval(['r_' h_name(3:end)])];    
            end
        end       
        evalc([char(parameters(ii)) '=par']);
    end
end

matfile.data.par1d    = [h_time h_ver h_Magic_const h_az h_el h_Pt...
    h_SCangle h_XMITloc h_RECloc h_Tsys h_code h_om0 h_m0...
    h_phasepush h_Offsetppd nh npprange];

matfile.data.par2d    = [h_h h_range h_param h_error h_apriori...
    h_apriorierror h_status h_dp h_res h_w];

matfile.data.par2d_pp = [h_pprange h_pp h_pperr h_ppw];

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
aa = find(cellfun('isempty',matfile.metadata.par0d(6,:)));    matfile.metadata.par0d(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par0d(7,:)));    matfile.metadata.par0d(7,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par1d(6,:)));    matfile.metadata.par1d(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par1d(7,:)));    matfile.metadata.par1d(7,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d(6,:)));    matfile.metadata.par2d(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d(7,:)));    matfile.metadata.par2d(7,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d_pp(6,:))); matfile.metadata.par2d_pp(6,aa)= {'0'};
aa = find(cellfun('isempty',matfile.metadata.par2d_pp(7,:))); matfile.metadata.par2d_pp(7,aa)= {'0'};
       
matfile.metadata.schemes.DataCite.Identifier = 'PID';
matfile.metadata.schemes.DataCite.Creator = 'Ingemar Häggström';
matfile.metadata.schemes.DataCite.Title = datafolder;
matfile.metadata.schemes.DataCite.Publisher = 'EISCAT Scientific Association';
matfile.metadata.schemes.DataCite.ResourceType = 'dataset/Level 3 Ionosphere';

if exist('name_sig')
    dd = strfind(name_sig,' ');
    publdate = name_sig(dd(1)+1:dd(2)-1);
    publdate = datestr(datenum(publdate,'dd-mmm-yyyy'),'yyyy-mm-dd'); % date on the form YYYY-MM-DD
    publyear = publdate(1:4);
    matfile.metadata.schemes.DataCite.Date = ['Created/' publdate];
    matfile.metadata.schemes.DataCite.PublicationYear = publyear'; 
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

% Generate an HDF5-file 
chunklim = 10;
sFields = fieldnames(matfile);
for sf = sFields.' 
    tFields = fieldnames(matfile.(char(sf)));
    for tf = tFields.'
        if strcmp('data',char(sf)) && (strcmp('par0d',char(tf)) || strcmp('par1d',char(tf)) || strcmp('par2d',char(tf)) || strcmp('par2d_pp',char(tf)) || strcmp('acf',char(tf)) || strcmp('ace',char(tf)) || strcmp('lag',char(tf)) || strcmp('freq',char(tf)) || strcmp('spec',char(tf)) || strcmp('om',char(tf)))
            npar  = length(matfile.data.(char(tf))(1,:));
            ndata = length(matfile.data.(char(tf))(:,1));
            if ge(ndata,chunklim) && ge(npar,chunklim), csize = [chunklim chunklim];
            elseif ge(ndata,chunklim), csize = [chunklim npar];
            elseif ge(npar,chunklim), csize = [ndata chunklim];
            else csize = [ndata npar]; end    
            h5create(hdffilename,['/' char(sf) '/' char(tf)],size([matfile.(char(sf)).(char(tf))]),'ChunkSize',csize,'Deflate',9);
            h5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
        elseif strcmp('metadata',char(sf)) 
            if ~exist(hdffilename)
                hdf5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
            else
                hdf5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))],'WriteMode','append'); 
            end
        else
            h5create(hdffilename,['/' char(sf) '/' char(tf)],size([matfile.(char(sf)).(char(tf))]));
            h5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
        end
    end
end