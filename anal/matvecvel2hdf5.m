% Generate an EISCAT HDF5-file from mat-files generated in a Guisdap analysis

%function [storepath,EISCAThdf5file] = mat2hdf5(matpath, datapath,addfigs,addnotes) 
global path_GUP result_path 

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

start_GUP
vecveldir = '/home/rikard/matlab/AnalysedData/2020-02-27_beata_60@vhf/vectors/';
matstruct = dir(fullfile(vecveldir,'*.mat'));
mat = fullfile(matstruct.folder,matstruct.name); 
load(mat)

datapath = vecveldir;
%matfile.data.utime = [posixtime(datetime(datestr(Vdate(1,:)'))) posixtime(datetime(datestr(Vdate(1,:)')))];

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
    matfile.data.utime = [posixtime(datetime(datestr(vdate(1,:)))) posixtime(datetime(datestr(vdate(1,:))))];
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
            field
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
    
%     starttime = datestr(r_gfd.t1,'yyyy-mm-ddTHH:MM:SS');
%     endtime   = datestr(r_gfd.t2,'yyyy-mm-ddTHH:MM:SS');
%     intper_med = median(r_gfd.intper);
%     matfile.metadata.gfd.intper_median = {num2str(intper_med)};

    starttime = datestr(t1,'yyyy-mm-ddTHH:MM:SS');
    endtime   = datestr(t2,'yyyy-mm-ddTHH:MM:SS');

% 
% if ~exist('starttime','var')
%     matfile_tmp = fullfile(matpath,filelist(1).name);
%     load(matfile_tmp)
%     starttime = datestr(r_time(1,:),'yyyy-mm-ddTHH:MM:SS');
%     matfile_tmp = fullfile(matpath,filelist(1).name);
%     load(matfile_tmp)
%     endtime = datestr(r_time(2,:),'yyyy-mm-ddTHH:MM:SS');
% end
%     

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


% if strcmp(matpath(end),'/')
%     matpath = matpath(1:end-1);
% end
% [~,matfolder] = fileparts(matpath);
% dd_  = strfind(matfolder,'_');
% ddat = strfind(matfolder,'@');
% dd = sort([dd_ ddat]);
% if length(dd)==4
%     name_expr_more = [matfolder(dd(2)+1:dd(3)-1) '_'];
% else
%     name_expr_more = [];
% end
% 
% ant = matfolder(dd(end)+1:end);
% if isempty(name_ant) 
%    switch name_site
%        case 'L', if isempty(str2num(ant)), name_ant = ant; else name_ant = 'esr'; end
%        case 'K', name_ant = 'kir';
%        case 'T', name_ant = 'uhf';
%        case 'S', name_ant = 'sod';
%        case 'V', name_ant = 'vhf';
%        case 'Q', name_ant = 'quj';
%    end
% end

datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_velvec_' '@' name_ant];
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

% % 0d and 1d parameters 
% parameters_1d = {'h_Magic_const' 'h_az' 'h_el' 'h_Pt' 'h_SCangle'...
%     'h_XMITloc' 'h_RECloc' 'h_Tsys' 'h_code' 'h_om0' 'h_m0' 'h_phasepush'...
%     'h_Offsetppd' 'h_gain' 'h_fradar'};
% % 2d parameters 
% parameters_2d = {'h_h' 'h_range' 'h_param' 'h_error' 'h_apriori'...
%     'h_apriorierror' 'h_status' 'h_dp' 'h_res' 'h_w'};
% % 2d_pp parameters 
% parameters_2dpp = {'h_pprange' 'h_pp' 'h_pperr' 'h_ppw'};

[~,text] = xlsread(GuisdapParFile);
parameters_list = text(:,1);    % list that includes all Guisdap parameters and keep their positions from the excel arc

matfile.metadata.header= text(1,1:7)';

% % % % unixtime
% % % timepar = [];
% % % ttt = 0;
% % % a = find(strcmp('h_time',parameters_list)==1);
% % % for jj = 1:2; ttt = ttt+1;
% % %     [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % %     info(1) = {['time' num2str(jj)]};
% % %     info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % %     matfile.metadata.utime(:,ttt) = info';
% % % end
% % % for jj = 1:rec
% % %      matfile_tmp = fullfile(matpath,filelist(jj).name);
% % %      load(matfile_tmp)
% % %      timepar = [timepar; posixtime(datetime(r_time(1,:))) posixtime(datetime(r_time(2,:)))];   % unix time
% % % end
% % % matfile.data.utime = timepar;
% % %             
% % % 
% % % nn1 = 0;
% % % [uniom0,unigain,unifradar] = deal(0);
% % % for ii = 1:length(parameters_1d)
% % %      h_name1 = char(parameters_1d(ii));
% % %      if ~exist(['r_' h_name1(3:end)],'var')
% % %          continue
% % % %      elseif strcmp(h_name1,'h_time')
% % % %          a = find(strcmp(h_name1,parameters_list)==1);
% % % %          for jj = 1:2; nn1 = nn1+1;
% % % %             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % % %             info(1) = {[h_name1(3:end) num2str(jj)]};
% % % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % % %             matfile.metadata.par1d(:,nn1) = info';
% % % %          end
% % %      elseif strcmp(h_name1,'h_XMITloc')
% % %          a = find(strcmp(h_name1,parameters_list)==1);
% % %          for jj = 1:3; nn1 = nn1+1; 
% % %             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % %             info(1) = {[h_name1(3:end) num2str(jj)]};
% % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % %             matfile.metadata.par1d(:,nn1) = info';
% % %          end
% % %      elseif strcmp(h_name1,'h_RECloc') 
% % %          a = find(strcmp(h_name1,parameters_list)==1);
% % %          for jj = 1:3; nn1 = nn1+1; 
% % %             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % %             info(1) = {[h_name1(3:end) num2str(jj)]};
% % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % %             matfile.metadata.par1d(:,nn1) = info';
% % %          end
% % %      elseif strcmp(h_name1,'h_om0') && length(unique(eval(['r_om0']))) == 1
% % %          nn1 = nn1+1;
% % %          uniom0 = 1;
% % %          a = find(strcmp(h_name1,parameters_list)==1);
% % %          [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %          info(1) = {[h_name1(3:end)]};
% % %          info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %          matfile.metadata.par1d(:,nn1) = info';
% % %      elseif strcmp(h_name1,'h_gain') && length(unique(eval(['r_gain']))) == 1
% % %          nn1 = nn1+1;
% % %          unigain = 1;
% % %          a = find(strcmp(h_name1,parameters_list)==1);
% % %          [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %          info(1) = {[h_name1(3:end)]};
% % %          info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %          matfile.metadata.par1d(:,nn1) = info';   
% % %      elseif strcmp(h_name1,'h_fradar') && length(unique(eval(['r_fradar']))) == 1
% % %          nn1 = nn1+1;
% % %          unifradar = 1;
% % %          a = find(strcmp(h_name1,parameters_list)==1);
% % %          [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %          info(1) = {[h_name1(3:end)]};
% % %          info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %          matfile.metadata.par1d(:,nn1) = info';    
% % %      elseif length(eval(['r_' h_name1(3:end)]))==1
% % %          nn1 = nn1+1;
% % %          a = find(strcmp(h_name1,parameters_list)==1);             
% % %          [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %          info(1) = {h_name1(3:end)};
% % %          info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))}; 
% % %          matfile.metadata.par1d(:,nn1) = info';
% % %      else
% % %          num = length(eval(['r_' h_name1(3:end)]));
% % %          a = find(strcmp(h_name1,parameters_list)==1);
% % %          [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %          info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %          for jj = 1:num; nn1 = nn1+1;
% % %              info(1) = {[h_name1(3:end) num2str(jj)]};  
% % %              matfile.metadata.par1d(:,nn1) = info';
% % %          end
% % %      end
% % %      
% % % end
% % % 
% % % % adding record lengths 
% % % nn1 = nn1 + 1;
% % % a = find(strcmp('nrec',parameters_list)==1);
% % % [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % % info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % % matfile.metadata.par1d(:,nn1) = info';
% % % 
% % % nn1 = nn1 + 1;
% % % a = find(strcmp('nrec_pp',parameters_list)==1);
% % % [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % % info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % % matfile.metadata.par1d(:,nn1) = info';
% % % 
% % % nn2 = 0;
% % % for ii = 1:length(parameters_2d)
% % %     h_name2 = char(parameters_2d(ii));
% % %     if ~exist(['r_' h_name2(3:end)],'var')
% % %         continue
% % %     end
% % %     dim=size(eval(['r_' h_name2(3:end)]));
% % %     if dim(2)==1
% % %         nn2 = nn2 + 1;
% % %         a = find(strcmp(h_name2,parameters_list)==1);
% % %         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %         info(1) = {h_name2(3:end)};
% % %         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %         matfile.metadata.par2d(:,nn2) = info';
% % %     elseif strcmp(h_name2,'h_param') || strcmp(h_name2,'h_error') || strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror')
% % %         a = find(strcmp(h_name2,parameters_list)==1);
% % %         if strcmp(h_name2,'h_apriori') || strcmp(h_name2,'h_apriorierror')
% % %             nump = length(r_apriori(1,:));
% % %         else
% % %             nump = length(r_param(1,:));
% % %         end
% % %         for jj = 1:5; nn2 = nn2 + 1; 
% % %             [~,~,info]  = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % %             matfile.metadata.par2d(:,nn2) = info';
% % %         end
% % %         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj+1) ':E' num2str(a+jj+1)]); info_tmp = info;
% % %         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj+1)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj+1)])) };
% % %         if ge(nump,6)
% % %             for kk=1:length(r_m0)-1; nn2 = nn2 + 1; 
% % %                 if  (length(r_m0)-1)>1
% % %                     info(1) = {[char(info_tmp(1)) num2str(kk)]};
% % %                 end
% % %                 if r_m0(kk)==30.5   
% % %                     if     strcmp(h_name2,'h_param'),        info(2)={'ion mix content: [O2+,NO+]/N'};                info(5)={'pm'};  info(6)={'690'};
% % %                     elseif strcmp(h_name2,'h_error'),        info(2)={'error ion mix content: [O2+,NO+]/N'};          info(5)={'dpm'}; info(6)={'-690'};
% % %                     elseif strcmp(h_name2,'h_apriori'),      info(2)={'a priori ion mix content: [O2+,NO+]/N'};       info(5)={'N/A'}; info(6)={'0'};
% % %                     elseif strcmp(h_name2,'h_apriorierror'), info(2)={'a priori error ion mix content: [O2+,NO+]/N'}; info(5)={'N/A'}; info(6)={'0'};
% % %                     end  
% % %                 elseif r_m0(kk)==16 
% % %                     if     strcmp(h_name2,'h_param'),        info(2)={'O+ content: [O+]/N'};                info(5)={'po+'};  info(6)={'620'};
% % %                     elseif strcmp(h_name2,'h_error'),        info(2)={'error O+ content: [O+]/N'};          info(5)={'dpo+'}; info(6)={'-620'};
% % %                     elseif strcmp(h_name2,'h_apriori'),      info(2)={'a priori O+ content: [O+]/N'};       info(5)={'N/A'};  info(6)={'0'};
% % %                     elseif strcmp(h_name2,'h_apriorierror'), info(2)={'a priori error O+ content: [O+]/N'}; info(5)={'N/A'};  info(6)={'0'};
% % %                     end
% % %                 end
% % %                 matfile.metadata.par2d(:,nn2) = info';
% % %             end
% % %             if isempty(kk); kk=0; end
% % %             for ll = (jj + 2):nump; nn2 = nn2 + 1;
% % %                 [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+ll) ':E' num2str(a+ll)]); 
% % %                 info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+ll)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+ll)]))};
% % %                 matfile.metadata.par2d(:,nn2) = info';
% % %             end
% % %         end
% % %         if strcmp(h_name2,'h_error')
% % %             a = find(strcmp('crossvar',parameters_list)==1);
% % %             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %             for jj = 1:nump-1
% % %                 for kk = 1:nump-jj; nn2 = nn2 + 1;
% % %                     info(1) = {['crossvar_' num2str(kk) num2str(kk+jj)]};
% % %                     info(2) = {['cross variance (p' num2str(kk) ',p' num2str(kk+jj) ')']};
% % %                     matfile.metadata.par2d(:,nn2) = info';
% % %                 end
% % %             end
% % %         end
% % %     elseif strcmp(h_name2,'h_res') 
% % %         a = find(strcmp(h_name2,parameters_list)==1);
% % %         for jj = 1:2; nn2 = nn2+1; 
% % %             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % %             info(1) = {[h_name2(3:end) num2str(jj)]};
% % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % %             matfile.metadata.par2d(:,nn2) = info';
% % %         end
% % %     elseif strcmp(h_name2,'h_w') 
% % %         a = find(strcmp(h_name2,parameters_list)==1);
% % %         for jj = 1:3; nn2 = nn2+1; 
% % %             [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a+jj) ':E' num2str(a+jj)]);
% % %             info(1) = {[h_name2(3:end) num2str(jj)]};
% % %             info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a+jj)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a+jj)]))};
% % %             matfile.metadata.par2d(:,nn2) = info';
% % %         end   
% % %     else 
% % %         a = find(strcmp(parameters_2d(ii),parameters_list)==1); 
% % %         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %         for jj = 1:dim(2); nn2 = nn2 + 1;
% % %             info(1) = {[h_name2(3:end) num2str(jj)]};       
% % %             matfile.metadata.par2d(:,nn2) = info';
% % %         end
% % %     end
% % % end
% % % 
% % % nn3 = 0;
% % % for ii = 1:length(parameters_2dpp)
% % %      h_name2pp = char(parameters_2dpp(ii));
% % %      if ~exist(['r_' h_name2pp(3:end)],'var')
% % %          continue
% % %      end
% % %      nn3 = nn3 + 1;
% % %      a = find(strcmp(h_name2pp,parameters_list)==1);
% % %      [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %      info(1) = {h_name2pp(3:end)};
% % %      info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))}; 
% % %      matfile.metadata.par2d_pp(:,nn3) = info';
% % % end
% % % 
% % % parameters = [parameters_1d parameters_2d parameters_2dpp];
% % % 
% % % nh = [];
% % % npprange = [];
% % % 
% % % for ii = 1:length(parameters)
% % %     h_name = char(parameters(ii));
% % %     if ~exist(['r_' h_name(3:end)],'var') 
% % %         evalc([h_name '=[]']); continue
% % %     else
% % %         par = []; 
% % %         for jj = 1:rec
% % %             matfile_tmp = fullfile(matpath,filelist(jj).name);
% % %             load(matfile_tmp)
% % % %             if strcmp(h_name,'h_time')
% % % %                 par = [par; posixtime(datetime(r_time(1,:))) posixtime(datetime(r_time(2,:)))];   % unix time
% % %             if strcmp(h_name,'h_Tsys')
% % %                 par = [par; eval(['r_' h_name(3:end)])'];                                                
% % %             elseif strcmp(h_name,'h_h')
% % %                 nh = [nh; length(r_h)];
% % %                 par = [par; eval(['r_' h_name(3:end)])];
% % %             elseif strcmp(h_name,'h_pprange')
% % %                 npprange = [npprange; length(r_pprange)];
% % %                 par = [par; eval(['r_' h_name(3:end)])];
% % %             elseif strcmp(h_name,'h_om0') && uniom0 == 1
% % %                 r_om0 = r_om0(1);  
% % %                 par = [par; eval(['r_' h_name(3:end)])]; 
% % %             elseif strcmp(h_name,'h_gain') && unigain == 1
% % %                 r_gain = r_gain(1);
% % %                 par = [par; eval(['r_' h_name(3:end)])]; 
% % %             elseif strcmp(h_name,'h_fradar') && unifradar == 1
% % %                 r_fradar = r_fradar(1);
% % %                 par = [par; eval(['r_' h_name(3:end)])];    
% % %             else
% % %                 par = [par; eval(['r_' h_name(3:end)])];    
% % %             end
% % %         end       
% % %         evalc([char(parameters(ii)) '=par']);
% % %     end
% % % end
% % % 
% % % matfile.data.par1d    = [h_Magic_const h_az h_el h_Pt...
% % %     h_SCangle h_XMITloc h_RECloc h_Tsys h_code h_om0 h_m0...
% % %     h_phasepush h_Offsetppd h_gain h_fradar nh npprange];
% % % 
% % % matfile.data.par2d    = [h_h h_range h_param h_error h_apriori...
% % %     h_apriorierror h_status h_dp h_res h_w];
% % % 
% % % matfile.data.par2d_pp = [h_pprange h_pp h_pperr h_ppw];
% % % 
% % % 
% % % parameters_special = {'h_om' 'h_lag' 'h_spec' 'h_freq' 'h_acf' 'h_ace'};
% % % 
% % % for ii = 1:length(parameters_special)
% % %     h_name = parameters_special{ii};
% % %     name = h_name(3:end);
% % %     if exist(['r_' name],'var') 
% % %         a = find(strcmp(parameters_special{ii},parameters_list)==1);
% % %         [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(a) ':E' num2str(a)]);
% % %         info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(a)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(a)]))};
% % %         if isempty(info{6}), info{6} = '0'; end
% % %         if isempty(info{7}), info{7} = '0'; end
% % %         matfile.metadata.(name) = info';
% % %         par = []; 
% % %         for jj = 1:rec
% % %             matfile_tmp = fullfile(matpath,filesep,filelist(jj).name);
% % %             load(matfile_tmp)
% % %             par = [par;  eval(['r_' name])'];
% % %             end      
% % %         matfile.data.(name) = par;
% % %     end
% % % end
% % % 
% % % k = length(matfile.data.par2d(1,:));
% % % n = length(matfile.data.par1d(1,:));
% % % 
% % % index = [];
% % % pp = 0;
% % % for ii = 1:k
% % %     if length(matfile.data.par2d(:,ii))==length(filelist)
% % %         pp = pp +1;
% % %         matfile.data.par1d(:,n+pp)     = matfile.data.par2d(:,ii);
% % %         matfile.metadata.par1d(:,n+pp) = matfile.metadata.par2d(:,ii);
% % %         index = [index ii];
% % %     end
% % % end
% % % matfile.data.par2d(:,index)     = [];
% % % matfile.metadata.par2d(:,index) = [];
% % % 
% % % n = length(matfile.data.par1d(1,:));    % now possibly a new length
% % % mm=0;
% % % index = [];
% % % 
% % % for ii = 1:n
% % %     if length(unique(round(matfile.data.par1d(:,ii),4)))==1 || sum(isfinite(matfile.data.par1d(:,ii))) == 0
% % %         mm = mm + 1;
% % %         value = matfile.data.par1d(1,ii);
% % %         matfile.data.par0d(mm) = value;
% % %         matfile.metadata.par0d(:,mm) = matfile.metadata.par1d(:,ii);
% % %         index = [index ii];
% % %     end
% % % end
% % % matfile.data.par1d(:,index)     = [];
% % % matfile.metadata.par1d(:,index) = [];

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
    a = find(strcmp('name_ant',parameters_list)==1); 
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_exps','var'); nn = nn + 1; 
    infoname(1) = {'name_exps'};
    infoname(2) = {name_exps};
    a = find(strcmp('name_expr',parameters_list)==1);                
    [~,~,infodesc] = xlsread(GuisdapParFile,1,['B' num2str(a)]);
    infoname(3) = infodesc;
    matfile.metadata.names(:,nn) = infoname';
end
if exist('name_ants','var'); nn = nn + 1; 
    infoname(1) = {'name_ants'};
    infoname(2) = {name_ant};
    a = find(strcmp('name_ant',parameters_list)==1); 
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
    sf
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
                                    strdata = matfile.(char(sf)).(char(tf)).(char(uf)).(char(vf)).(char(wf))
                                    dsname = char(wf)
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
addfigs = 1;
if addfigs
    image_filelist = [dir(fullfile(vecveldir,'*.png'));dir(fullfile(vecveldir,'*.pdf'))];
    npdf = 0;
    %if ~isempty(image_filelist)
      for ii = 1:length(image_filelist)
        figurefile = fullfile(vecveldir,image_filelist(ii).name);
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
addnotes = 1;
if addnotes
    notesfiles = dir(fullfile(vecveldir,'notes*txt'));
    for nn = 1:length(notesfiles)
        notesfile = fullfile(vecveldir,notesfiles(nn).name);
        addNote2Hdf5(notesfile,EISCAThdf5file,nn)
    end
end

%end

