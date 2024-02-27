% integr_syisr: Integration program for SYISR data stored as hdf5 files
% GUISDAP v9.2   22-02-15 Copyright EISCAT
%
% Integration program for voltage data in hdf5 files.
% Each call to this routine performs the integation for one integration period
%
% NOTE: This is a SYISR specific function
%
% Input parameters (all global):
% a_ind a_indold a_interval a_year a_start a_integr a_skip a_end
%     : specify the integration periods
% d_filelist : contains the names of the data files
% Output parameters:
% OK  : if true, the integration was succesful
% EOF : if true, the end of file was found during integration
% Output parameters (global):
% d_parbl : the parameter block returned by the integration program
% d_data : the integrated data vector
% d_var1 d_var2: data variances
%
% See also: an_start integr_data
% function  [OK,EOF,N_averaged,M_averaged]=integr_syisr
function  [OK,EOF,N_averaged,M_averaged]=integr_syisrw

global d_parbl d_data d_var1 d_var2 data_path d_filelist d_raw
global d_saveint
global a_ind a_interval a_year a_start a_integr a_skip a_end
global a_txlimit a_realtime a_txpower
global a_satch a_code a_intfix
persistent secs a_nnold a_beam d_dir d_ran d_profs d_beam d_ipp
persistent a_antold a_max a_posold a_averold fileslist

% processing logic: integrate data in a integrate interval, added by wyh

OK=0; EOF=0; N_averaged=0; M_averaged=0;
d_ExpInfo=[]; d_raw=[];

if a_ind == 0       % init, added by wyh
  a_ind = 1;
  a_cycle = sum(a_integr+a_skip);
  % remove files that dont in analysis time period, added by wyh 
  d = cell2mat({d_filelist.tai});
  d = find(d <= a_start | d > a_end);
  if length(d) == length(d_filelist)
    d=d(1:end-1);
  end
  d_filelist(d)=[];
  % skip to file start, added by wyh
  if a_cycle>0
    i=fix((d_filelist(1).tai-a_start)/a_cycle);
    if i>0, a_start=a_start+i*a_cycle; end
  end
  % init integrated interval, added by wyh
  a_interval=a_start+[0 a_integr(1)];
  % ?, added by wyh
  % a_max.gap=30; a_max.adiff=0.1*pi/180; %max 30s gap and 0.1 deg/s
  % a_posold=zeros(1,9);
  % a_nnold=zeros(1,9);
else
  a_indold=a_ind; a_ind=a_ind+1; if a_ind>length(a_integr), a_ind=1; end
  a_interval=a_interval(2)+a_skip(a_indold)+[0 a_integr(a_ind)];          % interval increment, added by wyh 
end

% this function stop condition, added by wyh 
if a_integr<=0, a_interval(2)=a_end;
elseif a_interval(2)>=a_end; EOF=1;
elseif d_filelist(end).tai<a_interval(1); EOF=1; return
end
% init fileslist var(that is tai list), added by wyh
if length(fileslist)~=length(d_filelist)
 fileslist=cell2mat({d_filelist.tai});
end
% files that in a_interval, added by wyh 
files=d_filelist(find(fileslist>a_interval(1) & fileslist<=a_interval(2)));
if isempty(files) & a_integr<=0, EOF=1; return, end

i = 0;
while i<length(files)
    % inc i and get file path, added by wyh 
    i=i+1; file=files(i);
    % init var, added by wyh 
    i_averaged=1; i_var1=[]; i_var2=[]; 
    
    % syisr phase II hdf5 file format, added by wyh
    syfile_name = file.fname;
    sytdfile_name = syfile_name;
    syfdfile_name = sytdfile_name;

    sytd_head = h5read(sytdfile_name, '/head');
    sytai_time = timeconv(double(sytd_head.dt)*1e-6-8*3600,'unx2tai');    % time is bei

    sytdlp_data = h5read(sytdfile_name, '/lag_profile');
    sytdlp_data = complex(double(sytdlp_data.r(:)), double(sytdlp_data.i(:)));

    sytdpp_data = h5read(sytdfile_name, '/power_profile');sytdpp_data = complex(double(sytdpp_data));
    sytdbac_data = h5read(sytdfile_name, '/bac_power_profile');sytdbac_data = complex(double(sytdbac_data));

    med = length(sytdbac_data) / 2;
    sytdbac_data = sytdbac_data(med-7:med+8);

    d_data = [sytdbac_data; sytdlp_data; sytdpp_data];   %; syfdbac_data; syfdlp_data];
   
    d_ExpInfo=sprintf('kst0 syisrac%d', 32);
    tvec = 1:6;                 % parameters holding time
    az = 10; el = 9;            % parameters for antenna pointing
    accumulated = 7;          % parameters which are accumulated
    inttime = 7;                % parameter that holds integration time
    non_negative = 9;         % parameters which are positive

    sypreint_time = sytd_head.nipp * sytd_head.ipp * 1e-6; %correct? int32*int32
    secs = sytai_time + double(sypreint_time);

    d_parbl(inttime) = sypreint_time;  % unit is s, added by wyh 
    d_parbl(tvec) = timeconv(secs, 'tai2utc');

    d_parbl(8) = 4e6; % hui hui
    d_parbl([az el]) = [sytd_head.az sytd_head.el];
    d_parbl(41) = 10;
    d_parbl(62) = 0; % to clear all upars

    % lagprofiler(),      not needed, added by wyh 

    a_inttime = d_parbl(inttime);
    d = find(d_parbl(non_negative)<=0);
    if ~isempty(d)
        d_parbl(non_negative(d)) = a_nnold(d);
    end
    a_nnold = d_parbl(non_negative);

    dumpOK=1;
    if ~isempty(a_satch)
        if ~isfield(a_satch,'prep'), a_satch.prep = a_inttime*1e6; end
        [dumpOK,ad_sat] = satch(d_parbl(el),secs,a_inttime);
    end
    if dumpOK
        if ~OK  % initialize with the first good dump
            data=zeros(size(d_data)); d_var1=data; d_var2=data; accum=0;
            starttime=secs-a_inttime;    % calculate starttime of first dump
            OK=1;
        end
        % update with the following files
        accum=accum+d_parbl(accumulated);
        M_averaged(2)=max(i_averaged);
        use=(1:length(d_data))';
        if ~isempty(a_satch) & ~isempty(ad_sat)
            i_averaged=i_averaged.*ones(size(use));
            i_averaged(ad_sat)=0;
            use(ad_sat)=[];
        end
        data(use)=data(use)+d_data(use);
        if isempty(i_var1)
            d_var1(use)=d_var1(use)+d_data(use).*d_data(use);
            d_var2(use)=d_var2(use)+d_data(use).*conj(d_data(use));
        else
            d_var1(use)=d_var1(use)+i_var1(use);
            d_var2(use)=d_var2(use)+i_var2(use);
        end
        N_averaged=N_averaged+i_averaged;
        M_averaged=sum(M_averaged);
        prev_parbl=d_parbl; % update previous parameter block
    elseif OK
        d_parbl=prev_parbl;
    end
    %catch, disp(lasterr), end
end

if OK % if at least one good data dump was found
    % update parameter block, accept the last parameter block as starting point
    d_parbl(accumulated)=accum;
    d_parbl(inttime)=timeconv(row(d_parbl(tvec)),'utc2tai')-starttime;
    d_data=data;
    M_averaged(1)=max(N_averaged);
    M_averaged(2)=min(N_averaged);
    if ~isempty(d_saveint)
        if all(M_averaged==1)
            i_averaged=[];
        else
            i_averaged=N_averaged; i_var1=real(d_var1); i_var2=d_var2;
        end
        d=timeconv(secs,'tai2gup');
        file=[d_saveint.dir sprintf('%08d.mat',fix(d(2)))];
        disp(file)
        if d_saveint.var
            save_noglobal(file,d_ExpInfo,d_parbl,d_data,d_raw,i_var1,i_var2,i_averaged)
        else
            if isfield(d_saveint,'range'), d_data=d_data(d_saveint.range); end
            save_noglobal(file,d_ExpInfo,d_parbl,d_data)
        end
    end
    if M_averaged(1)==0
        fprintf('Could not fill any data points: Skipping dumps\n')
        OK=0;
    end
end
