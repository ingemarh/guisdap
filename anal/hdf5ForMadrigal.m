% A script to create mat-structures (in the same manner as Bill Rideouts's code) 
% for kindat 6800 and 6801 separately from the Guisdap .hdf5-file, to go to
% Madrigal 3 as hdf5-files via install_experiment_25.py

% clear
% 
% dirpath = '/home/rikard/matlab/AnalysedData/2017-09-09_folke_60@32ma';
function hdf5ForMadrigal(dirpath)

%global path_GUP result_path
if nargin<1, dirpath = []; end
if isempty(dirpath), dirpath=result_path; end

if isstring(dirpath)
    dirpath = char(dirpath);    % need to be char class
end

if ~strcmp(dirpath(end),'/')  % the path must end with '/'
    dirpath = [dirpath '/'];
end

pathparts = strsplit(dirpath,filesep);     % separate the folders
exprfolder = pathparts{end-1};               % the last folder (experiment name)
%gupfile = fullfile(dirpath,'.gup');

kindat0 = 6800;
kindat1 = 6801;
AbsentData=-32767;
BadData=-32767;      % madrigal cannot handle +32767
ModelData=-32766;

filename  = fullfile(dirpath,['EISCAT_' exprfolder '.hdf5']);
filename0 = fullfile(dirpath,['MAD6800_' exprfolder '.mat']);         % name of the MADmat-file to be generated
filename1 = fullfile(dirpath,['MAD6801_' exprfolder '.mat']);  

if exist(filename,'file')==0
    warning(['The file ' filename ' does not exist! No hdf5:s for Madrigal generated...'])
    return
end

% new MADmat-file must not already exist
if exist(filename0)==2
    delete(filename0);
end
if exist(filename1)==2
    delete(filename1);
end

% Generating a mat-structure from the hdf5-file
matdata.metadata.par0d    = deblank(h5read(filename,'/metadata/par0d'));
matdata.metadata.par1d    = deblank(h5read(filename,'/metadata/par1d'));
matdata.metadata.par2d    = deblank(h5read(filename,'/metadata/par2d'));
matdata.metadata.par2d_pp = deblank(h5read(filename,'/metadata/par2d_pp'));

gfddata = h5read(filename,'/metadata/gfd');
gfdfields = fieldnames(gfddata);

for ii = 1:length(gfdfields)
    if ischar(gfddata.(char(gfdfields(ii)))')
        matdata.metadata.gfd.(char(gfdfields(ii))) = deblank(gfddata.(char(gfdfields(ii)))');
    else
        matdata.metadata.gfd.(char(gfdfields(ii))) = gfddata.(char(gfdfields(ii)))';
    end    
end

matdata.data.par0d    = h5read(filename,'/data/par0d')';
matdata.data.par1d    = h5read(filename,'/data/par1d');
matdata.data.par2d    = h5read(filename,'/data/par2d');
matdata.data.par2d_pp = h5read(filename,'/data/par2d_pp');

%[~,b]=fileparts(dirpath);
a = strfind(exprfolder,'@');
name_ant = exprfolder(a+1:a+3);

%%% time
time_id = 1;   
time_columns = find(str2num(char(matdata.metadata.par1d(end,:)))==time_id);
if isempty(time_columns)
    time_columns = find(str2num(char(matdata.metadata.par0d(end,:)))==time_id);
    time = matdata.data.par0d(:,time_columns);
else
    time = matdata.data.par1d(:,time_columns);
end

rectime_start = matdata.data.par1d(:,1);
rec = length(time(:,1));
t = datevec(datetime(time(:,1),'ConvertFrom','posixtime'));
year = t(1);

%%% metadata
meta_id = [2 65 66 67 68];  % [ver name_expr name_site name_ant name_sig]
for ii = meta_id
    column = find(str2num(char(matdata.metadata.par0d(end,:)))==ii);
    if ii == 2, ver = matdata.data.par0d(column); continue, end
    meta = char(matdata.metadata.par0d(1,column));
    if ii == 65, name_expr = meta; 
    elseif ii == 66, name_site = meta;
    elseif ii == 67, name_ant = meta;    
    elseif ii == 68, name_sig = meta;
    end
end



switch name_site
    case 'K', kinst=71;   if year>2012, kinst=75; end       % EISCAT Kiruna UHF (or VHF)
    case 'T', kinst=72;                                     % EISCAT Tromso UHF
    case 'S', kinst=73;   if year>2012, kinst=76; end       % EISCAT Sodankyla UHF (or VHF)
    case 'V', kinst=74;                                     % EISCAT Tromso VHF
    case 'L', kinst=95;                                    % EISCAT Svalbard Radar
    case 'Q', kinst=100;                                    % Quing IS radar
    otherwise,  kinst=AbsentData;
end


id = [4 5 6 7 10]; % [az, el, Pt, SCangle, Tsys]

for ii = id
    column = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
    if isempty(column)
        column = find(str2num(char(matdata.metadata.par0d(end,:)))==ii);
        if isempty(column)
            par_tmp = [];
        else
            for jj = 1:length(column)
                par_tmp(:,jj) = ones(rec,1)*matdata.data.par0d(:,column(jj));
            end
        end
    else
        for jj = 1:length(column)
            par_tmp(:,jj) = matdata.data.par1d(:,column(jj));
        end
    end
    if     ii == 4,  az = par_tmp;
    elseif ii == 5,  el = par_tmp;
    elseif ii == 6,  Pt = par_tmp;
    elseif ii == 7,  SCangle = par_tmp;
    elseif ii == 10, Tsys = median(par_tmp')';
    end
    par_tmp = [];
end

id = [18 20 21 22 23 24 54 28 29 30 31 32 55 53 56 19]; % [h,ne,ti,tr,collf,vi,po+,dnel,dti,dtr,dcol,dvo,res,status,w,range]

for ii = id
    column = find(str2num(char(matdata.metadata.par2d(end,:)))==ii);
    if isempty(column)
        column = find(str2num(char(matdata.metadata.par1d(end,:)))==ii);
        if isempty(column)
            par_tmp = [];
        else
            par_tmp = matdata.data.par1d(:,column);
        end
    else
        par_tmp = matdata.data.par2d(:,column); 
    end
    if     ii == 18, h = par_tmp;
    elseif ii == 20, ne = par_tmp;
    elseif ii == 21, ti = par_tmp;
    elseif ii == 22, tr = par_tmp;
    elseif ii == 23, collf = par_tmp;
    elseif ii == 24, vi = par_tmp;
    elseif ii == 54, dp = par_tmp;
    elseif ii == 28, dne = par_tmp;
    elseif ii == 29, dti = par_tmp;  
    elseif ii == 30, dtr = par_tmp;
    elseif ii == 31, dcollf = par_tmp;
    elseif ii == 32, dvi = par_tmp;
    elseif ii == 55, res = par_tmp(:,1);
    elseif ii == 53, status = par_tmp;
    elseif ii == 56, w = par_tmp(:,1);
    elseif ii == 19, range = par_tmp;
    end
    par_tmp = [];
end

id = [61 62 63 64];   % [pprange pp pperr ppw]

for ii = id
    column = find(str2num(char(matdata.metadata.par2d_pp(end,:)))==ii);
    if isempty(column)
        par_tmp = [];
    else
        par_tmp = matdata.data.par2d_pp(:,column);
    end
    if     ii == 61, pprange = par_tmp;
    elseif ii == 62, pp = par_tmp;
    elseif ii == 63, pperr = par_tmp;
    elseif ii == 64, ppw = par_tmp;
    end
    par_tmp = [];
end

%%% record lengths
nh_id = 70;
column = find(str2num(char(matdata.metadata.par1d(end,:)))==nh_id);
if isempty(column) 
    column = find(str2num(char(matdata.metadata.par0d(end,:)))==nh_id);
    nh = matdata.data.par0d(:,column);
    nh = nh*ones(1,rec);
else
    nh = matdata.data.par1d(:,column);
end

npprange_id = 71;
column = find(str2num(char(matdata.metadata.par1d(end,:)))==npprange_id);
if isempty(column) 
    column = find(str2num(char(matdata.metadata.par0d(end,:)))==npprange_id);
    npprange = matdata.data.par0d(:,column);
    npprange = npprange*ones(1,rec);
else
    npprange = matdata.data.par1d(:,column);
end




oneDParms  = cellstr(char('azm','elm','hsa','systmp','power'));

% Parameters for kindat = 6800
if ~isempty(ne)
        independent2DParms0 = cellstr(char('posn'));
        if strfind('KS',name_site)
            twoDParms0  = cellstr(char('gdalt','nel','ti','tr','col','vobi','po+','dnel','dti','dtr','dcol','dvobi','chisq','gfit'));
        elseif strfind('TVLQ',name_site) && ~isempty(w)
            twoDParms0  = cellstr(char('gdalt','nel','ti','tr','col','vo','po+','dnel','dti','dtr','dcol','dvo','chisq','gfit','altav'));
        else
            twoDParms0  = cellstr(char('gdalt','nel','ti','tr','col','vo','po+','dnel','dti','dtr','dcol','dvo','chisq','gfit'));
        end    
        %arraySplittingParms0 = cellstr(char());
        arraySplittingParms0 = {};
end  

% Parameters for kindat = 6801
if strfind('TVLQ',name_site) && ~isempty(pp)
    if ~isempty(pperr) && ~isempty(ppw)
        independent2DParms1  = cellstr(char('posn'));                                      % 120
        twoDParms1           = cellstr(char('range','popl','dpopl','beamid','rgate'));     % 505, -505, 147, 125
    else
        independent2DParms1  = cellstr(char('posn'));                                      % 120
        twoDParms1           = cellstr(char('range','popl','beamid'));                     % 505, 147
    end
    %arraySplittingParms1 = cellstr(char());
    arraySplittingParms1 = {};
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
madFile0 = MadrigalHdf5File(filename0, oneDParms, ...
          independent2DParms0, twoDParms0, arraySplittingParms0);
      
madFile1 = MadrigalHdf5File(filename1, oneDParms, ...
          independent2DParms1, twoDParms1, arraySplittingParms1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
nhalt_tmp    = 0;
npprange_tmp = 0;

for ii = 1:rec
    
    altrange = (nhalt_tmp+1):(nhalt_tmp+nh(ii));
    h_rec      = h(altrange);
    ne_rec     = ne(altrange);
    ti_rec     = ti(altrange);
    tr_rec     = tr(altrange);
    collf_rec  = collf(altrange);
    vi_rec     = vi(altrange);
    dp_rec     = dp(altrange);
    dne_rec    = dne(altrange);
    dti_rec    = dti(altrange);
    dtr_rec    = dtr(altrange);
    dcollf_rec = dcollf(altrange);
    dvi_rec    = dvi(altrange);
    res_rec    = res(altrange);
    status_rec = status(altrange);
    w_rec      = w(altrange);
    range_rec  = range(altrange);
    
    rangerange = (npprange_tmp+1):(npprange_tmp+npprange(ii));
    pprange_rec = pprange(rangerange);
    pp_rec      = pp(rangerange);
    pperr_rec   = pperr(rangerange);
    ppw_rec     = ppw(rangerange);
    
    if strfind('TVLQ',name_site) && ~isempty(pp)
         [pp_merged,pperr_merged,ppw_merged,pprange_merged,pprofile_id] = pp_merge(pp_rec,pperr_rec,ppw_rec,pprange_rec);  
    end
    
    numRows0 = nh(ii);
    numRows1 = length(pprange_merged);   % number of rows after merging
    
    ut1_unix = time(ii,1);  
    ut2_unix = time(ii,2);
    
    madFile0 = madFile0.appendRecord(ut1_unix, ut2_unix, ...
                 kindat0, kinst, numRows0);
    madFile1 = madFile1.appendRecord(ut1_unix, ut2_unix, ...
                 kindat1, kinst, numRows1);
            
    indexalt0 = 1:numRows0;
    indexalt1 = 1:numRows1;
           
       
    % Cast azimuth and elevation into range +-180, 0-90 degree
    if el(ii)>90
        m_el = 180 - el(ii);
        m_az = az(ii) + 180;
    else
        m_el = el(ii); m_az = az(ii);   
    end
        m_az = mod((m_az + 360),360);
    if m_az > 180
        m_az = m_az - 360;
    end
        
    nhalt_tmp = nhalt_tmp + nh(ii);
    npprange_tmp = npprange_tmp + npprange(ii);
    
    if ~isempty(ne)
        logNe    = real(log10(ne_rec));
        Ti       = real(ti_rec);
        Tr       = real(tr_rec);
        logINcol = real(log10(collf_rec));
        Vi       = real(-vi_rec);

        err_logNe    = real(log10(dne_rec));
        err_Ti       = real(dti_rec);
        err_Tr       = real(dtr_rec);
        err_logINcol = real(log10(dcollf_rec));
        err_Vi       = real(dvi_rec);
    
        % only output results for fit parameter 0
        d=find(status_rec~=0);
        logNe(d)        = BadData;
        Ti(d)           = BadData;
        Tr(d)           = BadData;
        logINcol(d)     = BadData;
        Vi(d)           = BadData;
        err_logNe(d)    = BadData;
        err_Ti(d)       = BadData;
        err_Tr(d)       = BadData;
        err_logINcol(d) = BadData;
        err_Vi(d)       = BadData;
   
        d=find(dne_rec==0);    err_logNe(d)    = ModelData;
        d=find(dti_rec==0);    err_Ti(d)       = ModelData;
        d=find(dtr_rec==0);    err_Tr(d)       = ModelData;
        d=find(dcollf_rec==0); err_logINcol(d) = ModelData;
        d=find(dvi_rec==0);    err_Vi(d)       = ModelData;
        
        % set 1D parameterslength(r_pprange_merged)
        madFile0 = madFile0.set1DParm('azm', m_az, madFile0.lastRecord);                % 130: Mean azimuth angle, [deg]
        madFile0 = madFile0.set1DParm('elm', m_el, madFile0.lastRecord);                % 140: Mean azimuth angle, [deg]
        madFile0 = madFile0.set1DParm('hsa', SCangle(ii)*180/pi, madFile0.lastRecord);  % 190: Half scattering angle (bistatic system), [deg]
        madFile0 = madFile0.set1DParm('systmp', Tsys(ii), madFile0.lastRecord);         % 482: System temperature, [K]
        madFile0 = madFile0.set1DParm('power', Pt(ii)/1000, madFile0.lastRecord);       % 486: Peak power, [kW]
                
        % set 2D parameters
        madFile0 = madFile0.set2DParm('posn', indexalt0, madFile0.lastRecord);
        madFile0 = madFile0.set2DParm('gdalt', h_rec, madFile0.lastRecord);             % 110: Geodetic altitude (height), [km]
      if strfind('TVLQ',name_site) && ~isempty(w)
            % Altitude averaging interval
            Earth_radius=6372;
            sin_el = sin(pi*el(ii)/180);
            ran1 = (range_rec-w_rec/2)/Earth_radius;
            ran2 = (range_rec+w_rec/2)/Earth_radius;
            hw = Earth_radius*(sqrt(1+2*ran2*sin_el+ran2.^2)-sqrt(1+2*ran1*sin_el+ran1.^2));
        
        madFile0 = madFile0.set2DParm('altav', hw, madFile0.lastRecord);           % 115: Altitude averaging interval, [km]
      end     
        madFile0 = madFile0.set2DParm('nel', logNe, madFile0.lastRecord);          % 520: log10 (electron density in m^-3), lg[m^-3]
        madFile0 = madFile0.set2DParm('ti', Ti, madFile0.lastRecord);              % 550: Ion temperature, [K]
        madFile0 = madFile0.set2DParm('tr', Tr, madFile0.lastRecord);              % 570: Temperature ratio (Te/ti), []
        madFile0 = madFile0.set2DParm('col', logINcol, madFile0.lastRecord);       % 720: log10 (ion-neutral collision frequency), lg[s^-1]
            if strfind('KS',name_site)
        madFile0 = madFile0.set2DParm('vobi', Vi, madFile0.lastRecord);            % 590: Bisector ion vel (bistatic sys;pos=up), [m/s]
            else
        madFile0 = madFile0.set2DParm('vo', Vi, madFile0.lastRecord);              % 580: Line of sight ion velocity (pos = away), [m/s]
            end
        madFile0 = madFile0.set2DParm('po+', dp_rec, madFile0.lastRecord);          % 620: Composition [O+]/Ne, []   
        madFile0 = madFile0.set2DParm('dnel', err_logNe, madFile0.lastRecord);     % -520: error log10 (electron density in m^-3), lg[m^-3]
        madFile0 = madFile0.set2DParm('dti', err_Ti, madFile0.lastRecord);         % -550: error Ion temperature, [K]
        madFile0 = madFile0.set2DParm('dtr', err_Tr, madFile0.lastRecord);         % -570: error Temperature ratio (Te/ti), []
        madFile0 = madFile0.set2DParm('dcol', err_logINcol, madFile0.lastRecord);  % -720: error log10 (ion-neutral collision frequency), lg[s^-1]
            if strfind('KS',name_site)
        madFile0 = madFile0.set2DParm('dvobi', err_Vi, madFile0.lastRecord);       % -590: error Bisector ion vel (bistatic sys;pos=up), [m/s]
            else
        madFile0 = madFile0.set2DParm('dvo', err_Vi, madFile0.lastRecord);         % -580: error Line of sight ion velocity (pos = away), [m/s]   
            end
        madFile0 = madFile0.set2DParm('chisq', res_rec, madFile0.lastRecord);       % 420: Reduced-chi square of fit, []
        madFile0 = madFile0.set2DParm('gfit', status_rec, madFile0.lastRecord);      % 430: Goodness of fit, []
       
    end

    
    %%%%%%%%%%%%%%%%%% kindat = 6801
    if strfind('TVLQ',name_site) && ~isempty(pp)
        
        % set 1D parameters
        madFile1 = madFile1.set2DParm('posn', indexalt1, madFile1.lastRecord);
        madFile1 = madFile1.set1DParm('azm', m_az, madFile1.lastRecord);                % 130: Mean azimuth angle, [deg]
        madFile1 = madFile1.set1DParm('elm', m_el, madFile1.lastRecord);                % 140: Mean azimuth angle, [deg]
        madFile1 = madFile1.set1DParm('hsa', SCangle(ii)*180/pi, madFile1.lastRecord);    % 190: Half scattering angle (bistatic system), [deg]
        madFile1 = madFile1.set1DParm('systmp', Tsys(ii), madFile1.lastRecord);   % 482: System temperature, [K]
        madFile1 = madFile1.set1DParm('power', Pt(ii)/1000, madFile1.lastRecord);         % 486: Peak power, [kW]
        
        % set 2D parameters
        
        madFile1 = madFile1.set2DParm('range', pprange_merged, madFile1.lastRecord);               % 120: Range, [km]
        madFile1 = madFile1.set2DParm('beamid', pprofile_id, madFile1.lastRecord);                 % 147
        madFile1 = madFile1.set2DParm('popl', real(log10(pp_merged)), madFile1.lastRecord);        % 505: Log10(uncorrected electron density), lg[m^-3]
            if ~isempty(pperr) && ~isempty(ppw)
        madFile1 = madFile1.set2DParm('dpopl', real(log10(pperr_merged)), madFile1.lastRecord);    % -505: error Log10(uncorrected electron density), lg[m^-3]
        madFile1 = madFile1.set2DParm('rgate', ppw_merged, madFile1.lastRecord);                   % 125: Width of range gate, [km]    
            end
        
    end
end

%

principleInvestigator = 'Ingemar Haeggstroem';
expPurpose = '';
expMode = name_expr;
cycleTime = [];
correlativeExp = ''; 
sciRemarks = ''; 
instRemarks = '';

madFile0 = madFile0.setCatalog(principleInvestigator, expPurpose, expMode, ...
                            cycleTime, correlativeExp, sciRemarks, instRemarks);
madFile1 = madFile1.setCatalog(principleInvestigator, expPurpose, expMode, ...
                            cycleTime, correlativeExp, sciRemarks, instRemarks);
                        
% Header input                         
kindatDesc0 = 'Guisdap analysed profiles';  
kindatDesc1 = 'Guisdap power profiles'; 
analyst = name_sig; 
comments = ['Guisdap version ' num2str(ver)];
history = '';

madFile0 = madFile0.setHeader(kindatDesc0, analyst, comments, history);
madFile1 = madFile1.setHeader(kindatDesc1, analyst, comments, history);

write(madFile0);
write(madFile1);

end