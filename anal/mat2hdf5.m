% Generate a .hdf5 file for a Guisdap analysis

function mat2hdf5(dirpath)
    
global path_GUP result_path
if nargin<1, dirpath = []; end
if isempty(dirpath), dirpath=result_path; end

if isstring(dirpath)
    dirpath = char(dirpath);    % need to be char class
end

if ~strcmp(dirpath(end),'/')  % the path must end with '/'
    dirpath = [dirpath '/'];
end
    
pathparts = strsplit(dirpath,filesep);   
exprfolder = pathparts{end-1};                          

Hdf5File = sprintf('EISCAT_%s%s',exprfolder,'.hdf5');
MatFile = sprintf('MAT_%s%s',exprfolder,'.mat');
hdffilename = fullfile(dirpath,Hdf5File);
matfilename = fullfile(dirpath,MatFile);

GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file

if exist(hdffilename)==2, delete(hdffilename); end

filelist   = getfilelist(fullfile(dirpath));
rec        = length(filelist);
gupfile    = fullfile(dirpath,'.gup');

matfile_1 = fullfile(dirpath,filesep,sprintf('%08d%s',filelist(1).file,filelist(1).ext));    
load(matfile_1);

parameters_1d = {'h_time' 'h_ver' 'h_Magic_const' 'h_az' 'h_el' 'h_Pt' 'h_SCangle'...  % 0d and 1d parameters 
    'h_XMITloc' 'h_RECloc' 'h_Tsys' 'h_code' 'h_om0' 'h_om' 'h_m0' 'h_phasepush'...
    'h_Offsetppd' 'h_lag'};

parameters_2d = {'h_h' 'h_range' 'h_param' 'h_error' 'h_apriori'...                     % 2d parameters
    'h_apriorierror' 'h_status' 'h_dp' 'h_res' 'h_w' 'h_spec' 'h_freq'...
    'h_ace' 'h_acf'};

parameters_2dpp = {'h_pprange' 'h_pp' 'h_pperr' 'h_ppw'};                           % 2d pp-parameters

[~,text] = xlsread(GuisdapParFile);     
parameters_list = text(:,1);                                                % list that includes all Guisdap parameters and keep their positions from the excel arc

matfile.metadata.header= text(1,1:7)';

nn1 = 0;
for ii = 1:length(parameters_1d)
     h_name1 = char(parameters_1d(ii));
     if ~exist(['r_' h_name1(3:end)],'var')
         continue
     elseif strcmp(h_name1,'h_time')
         a = find(strcmp(h_name1,parameters_list)==1);                         % find the row number in the xlsx-file corresponding to parameter(ii)
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
             info(1) = {[h_name1(3:end) num2str(jj)]};                                % rename variable (adding a number)
             if jj>1, info(5) = {''}; end
             matfile.metadata.par1d(:,nn1) = info';
         end
     end
     
end

% addition of record lengths 
nn1 = nn1 + 1;
a = find(strcmp('nh',parameters_list)==1);
[~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
matfile.metadata.par1d(:,nn1) = info';

nn1 = nn1 + 1;
a = find(strcmp('npprange',parameters_list)==1);
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
            if  (length(r_m0)-1)==1
                if r_m0(kk)==30.5;   info(2)={'ion mix content: [O2+,NO+]/N'}; info(5)={'dpm'};
                elseif r_m0(kk)==16; info(2)={'O+ content: [O+]/N'}; info(5)={'dpo+'};
                end
                matfile.metadata.par2d(:,nn2) = info';
            else
                info(1) = {[char(info_tmp(1)) num2str(kk)]};
                if r_m0(kk)==30.5;   info(2)={'ion mix content: [O2+,NO+]/N'}; info(5)={'dpm'};
                elseif r_m0(kk)==16; info(2)={'O+ content: [O+]/N'}; info(5)={'dpo+'};
                end
                matfile.metadata.par2d(:,nn2) = info';
            end  
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
            matfile_tmp = fullfile(dirpath,filesep,sprintf('%08d%s',filelist(jj).file,filelist(jj).ext));
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
    h_SCangle h_XMITloc h_RECloc h_Tsys h_code h_om0 h_om h_m0...
    h_phasepush h_Offsetppd h_lag nh npprange];

matfile.data.par2d    = [h_h h_range h_param h_error h_apriori...
    h_apriorierror h_status h_dp h_res h_w h_spec h_freq h_ace h_acf];

matfile.data.par2d_pp = [h_pprange h_pp h_pperr h_ppw];

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

if exist('name_expr','var'); mm = mm + 1; 
    a = find(strcmp('name_expr',parameters_list)==1);                
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(1) = {name_expr};
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par0d(:,mm) = info';
end
if exist('name_site','var'); mm = mm + 1; 
    a = find(strcmp('name_site',parameters_list)==1);                
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(1) = {name_site};
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par0d(:,mm) = info';
end
if exist('name_ant','var'); mm = mm + 1; 
    a = find(strcmp('name_ant',parameters_list)==1);                
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(1) = {name_ant};
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par0d(:,mm) = info';
    end
if exist('name_sig','var'); mm = mm + 1; 
    a = find(strcmp('name_sig',parameters_list)==1);                
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
    info(1) = {name_sig};
    info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
    matfile.metadata.par0d(:,mm) = info';
end
 
% store the gfd content
if exist('r_gfd','var')
    matfile.metadata.gfd = r_gfd;
    extra=r_gfd.extra;
else
    load('-mat',gupfile);    
    matfile.metadata.gfd.name_expr   = name_expr;
    matfile.metadata.gfd.expver      = expver;
    matfile.metadata.gfd.siteid      = siteid;
    matfile.metadata.gfd.data_path   = data_path;
    matfile.metadata.gfd.result_path = result_path; 
    matfile.metadata.gfd.intper      = intper;
    matfile.metadata.gfd.t1          = t1;
    matfile.metadata.gfd.t2          = t2;
    matfile.metadata.gfd.rt          = rt;
    matfile.metadata.gfd.figs        = figs;
    matfile.metadata.gfd.path_exps   = path_exps;
end
matfile.metadata.gfd.extra=row([extra ones(size(extra,1))*'#']');

%save(matfilename,'matfile')

% Make a .hdf5-file 
sFields = fieldnames(matfile);
for sf = sFields.' 
    tFields = fieldnames(matfile.(char(sf)));
    for tf = tFields.'
        if ~exist(hdffilename)
            hdf5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))]);
        else
            hdf5write(hdffilename,['/' char(sf) '/' char(tf)],[matfile.(char(sf)).(char(tf))],'WriteMode','append'); 
        end
    end
end