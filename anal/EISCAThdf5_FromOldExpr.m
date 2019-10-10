% Generate a .hdf5 file from old madrigal .hdf5-files, called by TheSuperMasterScript

% clear
% % hdf5file = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/03dec85/overview/12030902.tra.hdf5';
% % datapath = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/03dec85/ForMadrigal';
% hdf5file = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/04dec85/overview/12040000.kra.hdf5';
% datapath = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/04dec85/ForMadrigal';
% % hdf5file = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/11dec85/overview/12110001.sra.hdf5';
% % datapath = '/home/rikard/matlab/Guisdap/hdf5/NCARtoHdf5/11dec85/ForMadrigal';
% if exist(datapath)
%     rmdir(datapath,'s')
% end
% mkdir(datapath);

function storepath = EISCAThdf5_FromOldExpr(hdf5file,datapath)

global path_GUP newhdf5file

analysis_link = 'https://git.eiscat.se/eiscat/on-an';
matfile.metadata.analysis_link = analysis_link;


if nargin<1
    error('A .hdf5 (or .hdf) file is needed as input.')
end


[path,filename,ext] = fileparts(hdf5file);

if ~strcmp(ext,'.hdf5') && ~strcmp(ext,'.hdf')
    error('The input file is not an .hdf5 or .hdf.')
end

% dirpath = fullfile(path,filesep);
% path_GUP = '/home/rikard/matlab/Guisdap/guisdap9';

datapar = h5read(hdf5file,'/Metadata/Data Parameters');
data    = h5read(hdf5file,'/Data/Table Layout');
exprpar = h5read(hdf5file,'/Metadata/Experiment Parameters');

exprparnames = cellstr(exprpar.name');
exprparvalues = cellstr(exprpar.value');

for ii = 1:length(exprparnames)
    matfile.metadata.experiment.(char(regexprep(exprparnames(ii),' ','_'))) = exprparvalues{ii};
end

aa = find(strcmp(exprparnames,'Cedar file name')==1);
bb = find(strcmp(exprparnames,'kindat')==1);

cedarfile = char(exprparvalues(aa,:));

kindat_values = char(exprparvalues(bb,:));
kindats = str2num(char(strsplit(kindat_values,', ')'));
nkindats = length(kindats);
evenkindat = num2str(max(kindats));

pathparts = strsplit(cedarfile,filesep);
% [path,filename,ext]=fileparts(cedarfile);   % probably use this path in the final version, in order to store the new mat-files in the same old folder 
% [path,~,~]=fileparts(file);                 % probaly delete this path in the final version 
site = char(pathparts(end-2));
parnames = datapar.mnemonic';
npar = size(parnames,1);

year  = num2str(data.year(1));
month = num2str(data.month(1));
day   = num2str(data.day(1));
recs = length(data.ut1_unix);                            % # of records
intper_med = median(data.ut2_unix-data.ut1_unix);        % median integration time

if str2num(month)<10, month = ['0' month]; end
if str2num(day)<10, day = ['0' day]; end
display(['The site is ' site ' (' year ') and contains kindat ' kindat_values])

aa = find(strcmp(exprparnames,'instrument')==1);
if ~isempty(strfind(char(exprparvalues(aa)),'Kiruna')), name_ant = 'kir'; 
elseif ~isempty(strfind(char(exprparvalues(aa)),'Sodankyl')), name_ant = 'sod';
elseif ~isempty(strfind(char(exprparvalues(aa)),'Svalbard')), name_ant = 'esr';
elseif ~isempty(strfind(char(exprparvalues(aa)),'combined')), name_ant = 'esa'; error('name_ant = esa: Abort!! Abort!!')
elseif ~isempty(strfind(char(exprparvalues(aa)),'UHF')), name_ant = 'uhf';
else name_ant = 'vhf'; end


name_expr = ['cp' evenkindat(2) lower(char(96 + str2num(evenkindat(3:4))/2))];
matfile.metadata.experiment.name_expr = name_expr; 
matfile.metadata.experiment.intper_med = intper_med;

datafolder = ['EISCAT_' year '-' month '-' day '_' name_expr '_' num2str(intper_med) '@' name_ant];
storepath = fullfile(datapath,datafolder);
if exist(storepath)
   rmdir(storepath,'s');
end
mkdir(storepath);

Hdf5File = [datafolder '.hdf5'];
%Hdf5File = ['EISCAT_' year '-' month '-' day '_' name_expr '@' name_ant '.hdf5'];
MatFile =  ['MAT_' year '-' month '-' day '_' name_expr '@' name_ant '.mat'];
hdffilename = fullfile(storepath,Hdf5File);
matfilename = fullfile(storepath,MatFile);
newhdf5file = hdffilename;


% Hdf5File = ['EISCAT_' year '-' month '-' day '_' name_expr '@' lower(instr) '.hdf5'];
% MatFile =  ['MAT_' year '-' month '-' day '_' name_expr '@' lower(instr) '.mat'];


GuisdapParFile = fullfile(path_GUP,'matfiles','Guisdap_Parameters.xlsx'); % path to the .xlsx file

[~,text] = xlsread(GuisdapParFile);     
parameters_list = text(:,5);                            % list that includes all Guisdap parameters and keep their positions from the excel arc
gupparameters_list = text(:,1);

if exist(hdffilename)==2, delete(hdffilename); end

%gupfile    = fullfile(dirpath,'.gup');

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
    Parsinfile_list{aa}= 'acf';                                           % rename: sracf0 --> racf0    ( =sracf0.*sfacf0 )
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


matfile.data.par0d = [];
matfile.data.par1d = [];
matfile.metadata.par0d = [];
matfile.metadata.par1d = [];

matfile.data.par2d = [];
matfile.metadata.par2d = [];


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

if nkindats>1
    cckind = find(data.kindat == str2num(evenkindat));   % data.kindat does not seem to exist when there is only 1 kindat for the experiment
else cckind = 1:length(data.recno);
end
for ii = 1:npar
    
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
        end
        
        if length(unique(parameterdata))==1
            matfile.data.par0d = [matfile.data.par0d single(parameterdata(1))];
            [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
            if cell2mat(strfind(info(1),'h_'))==1
                info{1} = info{1}(3:end);
            end
            info(4) = {'-'};
            info(6:7) = {num2str(xlsread(GuisdapParFile,1,['F' num2str(aa)])) num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
            matfile.metadata.par0d = [matfile.metadata.par0d info'];
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
                aa = bb(jj); end                                     % since there are two 'range' in parameters_list there will be two aa:s. This bit of code requires the 'normal' one to be before the pp in the list.  
            
            if strcmp('vobi',char(Parsinfile_list(ii))),  aa = find(strcmp('vo',parameters_list)==1); end
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
                matfile.data.par1d = [matfile.data.par1d double(par_1d)]; 
                matfile.metadata.par1d = [matfile.metadata.par1d info'];
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
if cc3, matfile.data.par0d = [matfile.data.par0d str2num(exprparvalues{cc3})];
    aa = find(strcmp('RECloc3',gupparameters_list)==1);
    [~,~,info] = xlsread(GuisdapParFile,1,['A' num2str(aa) ':E' num2str(aa)]);
    info(4) = {'-'}; info(6:7) = {'0' num2str(xlsread(GuisdapParFile,1,['G' num2str(aa)]))};
    matfile.metadata.par0d = [matfile.metadata.par0d info'];
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


save(matfilename,'matfile')

% create a .hdf5
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
