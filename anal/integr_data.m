% integr_data: Integration program for EISCAT data stored as Matlab files
% GUISDAP v1.81   03-01-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% Integration program for radar data in Matlab files. The files may either contain
% individual dumps or integrated dumps. The latter may or may not have 
% the variance estimates of the data included.
% Each call to this routine performs the integation for one integration period
%
% NOTE: This is a EISCAT specific function
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
% See also: an_start integrate
% function  [OK,EOF]=integr_data
function  [OK,EOF,N_averaged,M_averaged]=integr_data()
 
global d_parbl d_data d_var1 d_var2 data_path d_filelist d_raw
global d_saveint
global a_ind a_interval a_year a_start a_integr a_skip a_end 
global a_txlimit a_realtime a_satch a_txpower
global a_intfixed a_intallow a_intfixforce a_intfix
global a_lpf
persistent a_antold a_max secs a_posold a_nnold a_averold fileslist

OK=0; EOF=0; jj=0; N_averaged=0; M_averaged=0;
d_ExpInfo=[]; d_raw=[]; txdown=0;
if a_ind==0
  a_ind=1;
  a_cycle=sum(a_integr+a_skip);
  d=cell2mat({d_filelist.tai});
  d=find(d<=a_start | d>a_end);
  if length(d)==length(d_filelist)
    d=d(1:end-1);
  end
  d_filelist(d)=[];
  if a_cycle>0
    i=fix((d_filelist(1).tai-a_start)/a_cycle);
    if i>0, a_start=a_start+i*a_cycle; end
  end
  a_interval=a_start+[0 a_integr(1)];
  a_max.gap=30; a_max.adiff=0.1*pi/180; %max 30s gap and 0.1 deg/s
  a_posold=zeros(1,9);
  a_nnold=zeros(1,9);
else
  a_indold=a_ind; a_ind=a_ind+1; if a_ind>length(a_integr), a_ind=1; end
  a_interval=a_interval(2)+a_skip(a_indold)+[0 a_integr(a_ind)];
end
if a_integr<=0, a_interval(2)=a_end;
elseif a_interval(2)>=a_end; EOF=1; end
if a_realtime
  if d_filelist(end).tai<=a_interval(1)
    [EOF,jj]=update_filelist(EOF,jj);
    if EOF, return, end
  end
elseif d_filelist(end).tai<a_interval(1)
  EOF=1; return
end

if length(fileslist)~=length(d_filelist)
 fileslist=cell2mat({d_filelist.tai});
end
fileform='%08d%s';
%if any(rem(fileslist,1)), fileform='%012.3f%s'; end
files=d_filelist(find(fileslist>a_interval(1) & fileslist<=a_interval(2)));
if isempty(files) & a_integr<=0, EOF=1; return, end
i=0;
while i<length(files)
  i=i+1; file=files(i);
  i_averaged=1; i_var1=[]; i_var2=[];
  if isfield(file,'ext') %.mat files
    filename=fullfile(file.dir,sprintf(fileform,file.file,file.ext));
    q_dir=dir(canon(filename,0));
    if a_realtime & now-datenum(q_dir.date)<1e-4
%     Check that the data file is old enough as it might still be being written to!
      pause(1)
    end
    try, load(canon(filename,0))
    catch,end
  else %hdf5 files
    d_parbl=h5read(file.fname,sprintf('/Data/%08d/Parameters',file.file));
    d_data=h5read(file.fname,sprintf('/Data/%08d/L2',file.file));
    d_data=complex(double(d_data.r(:)),double(d_data.i(:)));
    try
      d_ExpInfo=['kst0 ' char(h5read(file.fname,'/MetaData/ExperimentName'))];
      d_raw=h5read(file.fname,sprintf('/Data/%08d/L1',file.file));
      d_raw=complex(d_raw.r(:),d_raw.i(:));
    catch,end
  end
  if ~isempty(d_raw) && a_lpf(1).do
   lagprofiler()
  end

  lpb=length(d_parbl);
  if lpb==128
    [d_parbl,lpb]=nd2eros4(col(d_parbl));
  else
    d_parbl=d_parbl(:);
  end
  tvec=1:6;                    % parameters holding time
  fixed=a_intfix;              % parameters which are not allowed to change
  allow=a_intallow';           % max allowed change of fixed pars
  az=10; el=9;                 % parameters for antenna pointing
  averaged=[8:10 42 63 65];    % parameters which are averaged
  accumulated=[7 22 64];       % parameters which are accumulated
  inttime=7;                   % parameter that holds integration time
  positive=[8 65];             % parameters which are positive
  non_negative=[9 42 63];      % parameters which are positive
  if lpb>40 & d_parbl(41)==3   % for vhf antenna
   non_negative=[9 10 42 63];
   averaged=[8:10 42 63 70 75];
  end
  % do not work on unavailable parameters
  averaged(find(averaged>lpb))=[];
  allow(find(fixed>lpb))=[];
  fixed(find(fixed>lpb))=[];
  accumulated(find(accumulated>lpb))=[];
  positive(find(positive>lpb))=[];
  non_negative(find(non_negative>lpb))=[];

  secs1=timeconv(row(d_parbl(tvec)),'utc2tai');
  a_inttime=d_parbl(inttime);
  d=find(d_parbl(positive)<0);
  if ~isempty(d)
   d_parbl(positive(d))=a_posold(d);
  end
  a_posold=d_parbl(positive);
  d=find(d_parbl(non_negative)<=0);
  if ~isempty(d)
   d_parbl(non_negative(d))=a_nnold(d);
  end
  a_nnold=d_parbl(non_negative);
  a_ant=d_parbl([el az])/180*pi;
  [dx,dy,dz]=sph2cart(a_ant(2),a_ant(1),1); a_ant=[dx dy dz];
  d=d_parbl(averaged);
  if isempty(a_antold)
    a_antold=a_ant;
    secs=secs1;
    a_averold=d;
    N_averaged=0; M_averaged=0;
  end
  a_tx=d_parbl(a_txpower(1))*a_txpower(2);
  % At EISCAT we sample some parameters at the end of dump
  tdiff=(round(secs1/a_inttime)-round(secs/a_inttime))*a_inttime;
  if tdiff==a_inttime
    d_parbl(averaged)=(d+a_averold)/2;
  end
  a_averold=d;
  if a_integr<=0
    tdiff=tdiff>a_max.gap;
    if a_integr<0 & exist('starttime','var')
      tdiff=tdiff | starttime-secs1<a_integr;
    end
    adiff=atan2(norm(cross(a_ant,a_antold)),dot(a_ant,a_antold))/a_inttime;
    a_antold=a_ant;
    if adiff>a_max.adiff | tdiff
      a_interval(2)=file.tai;
      if tdiff & M_averaged>1
        a_interval(2)=file.tai-.5; a_antold=[];
      elseif M_averaged==0
        secs=secs1; return
      else
        secs=secs1;
      end
      d_parbl=prev_parbl;
      break
    end
    allow(find(fixed==az))=Inf;
    allow(find(fixed==el))=Inf;
  end

  secs=secs1;
  if secs<file.tai | file.tai-secs>=1
    fprintf('Filename %08d conflicts with time inside file: %.0f %d\n',file.file,timeconv(secs,'tai2gup')')
  end

  if isreal(d_data) % change to complex
    lendata=length(d_data);
    d_data(lendata-1:lendata)=abs(d_data(lendata-1:lendata));
    d_data=d_data(1:2:lendata)+sqrt(-1)*d_data(2:2:lendata);
  end

  dumpOK=(a_tx>=a_txlimit);
  if ~dumpOK
    if ~txdown, fprintf('Tx down\n'), txdown=1; end
  elseif ~isempty(a_satch)
    [dumpOK,ad_sat]=satch(d_parbl(el),secs,a_inttime);
  end
  if ~isempty(a_intfixforce)
    d=find(isfinite(a_intfixforce));
    if find(abs(d_parbl(fixed(d))-a_intfixforce(d))>allow(d))
      fprintf('Parameter bad:')
      fprintf(' par(%d)=%g[%g];',[fixed(d);d_parbl(fixed(d))';a_intfixforce(d)'])
      fprintf('\n')
      dumpOK=0;
    end
  end
  if dumpOK & OK
    indfixed=fixed(find(abs(d_parbl(fixed)-prev_parbl(fixed))>allow));
    if ~isempty(indfixed)
      fprintf('Parameter change:')
      fprintf(' par(%d)=%g[%g];',[indfixed;d_parbl(indfixed)';prev_parbl(indfixed)'])
      fprintf('\n')
      if a_intfixed
       fprintf('analysis_intfixed set: ')
       if M_averaged==1
        fprintf('Skipping previous dump\n')
        OK=0; N_averaged=0; M_averaged=0;
       else
        fprintf('Skipping dump\n')
        dumpOK=0;
       end
      end
    end
  end
  if dumpOK
    if ~OK  % initialize with the first good dump
      aver=0; data=zeros(size(d_data)); d_var1=data; d_var2=data; accum=0;
      starttime=secs-a_inttime;    % calculate starttime of first dump
      OK=1;
    end
    % update with the following files
    accum=accum+d_parbl(accumulated);
    M_averaged(2)=max(i_averaged);
    aver=aver+d_parbl(averaged)*M_averaged(2);
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
  if a_realtime & file.file==d_filelist(end).file
    [EOF,jj]=update_filelist(EOF,jj);
    if EOF, break, end
    d=cell2mat({d_filelist.tai});
    files=d_filelist(find(d>a_interval(1) & d<=a_interval(2)));
  end
  %catch, disp(lasterr), end
end

if OK, % if at least one good data dump was found
  % update parameter block, accept the last parameter block as starting point
  d_parbl(averaged)=aver/M_averaged;
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

function [EOF,jj]=update_filelist(EOF,jj)
global di_figures data_path b d_filelist a_end a_year
j=0;
if ~isempty(b) & ishandle(b(7))
 a_max_rtwait=a_end-d_filelist(end).tai; mrw=30;
else
 a_max_rtwait=300; mrw=round(sqrt(a_max_rtwait));
end
if ~jj
  if length(di_figures)>4 & di_figures(5)
    try, vizu rtgup; catch, rethrow(lasterror), end
  end
  jj=1; send_www
end

df=getfilelist(data_path,d_filelist(end));
while isempty(df)
  fprintf('.'); pause(mrw)
  [df,msg]=getfilelist(data_path,d_filelist(end));
  j=j+1;
  if j>a_max_rtwait/mrw
    msg=sprintf('No new data in %ds',a_max_rtwait);
  elseif ~isempty(b)
   if ishandle(b(7)) & get(b(7),'value')
    if j==10
     fprintf('Release RT button to finish analysis');
    end
   else
    msg='Aborting RT';
   end
  end
  if ~isempty(msg)
    fprintf('\n%s',msg)
    EOF=1; break
  end
end
d=cell2mat({df.file});
d=num2cell(timeconv([a_year*ones(size(d)) d],'gup2tai'));
[df.tai]=d{:};
d_filelist=[d_filelist;df];
if j>0, fprintf('\n'); end
