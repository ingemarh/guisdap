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
function  [OK,EOF,N_averaged]=integr_data(txlim)
 
global d_parbl d_data d_var1 d_var2 data_path d_filelist a_control 
global d_saveintdir
global a_ind a_interval a_year a_start a_integr a_skip a_end 
global a_txlim a_realtime a_satch
global a_antold a_txold a_elold a_maxgap secs a_intfixed
 
OK=0; EOF=0; jj=0; N_averaged=0;
d_ExpInfo=[]; d_raw=[];
if a_ind==0
  a_ind=1;
  a_cycle=sum(a_integr+a_skip);
  if a_cycle>0
    i=fix((d_filelist(1)-a_start)/a_cycle);
    if i>0, a_start=a_start+i*a_cycle; end
  end
  a_interval=a_start+[0 a_integr(1)];
  a_oldtime=0;
  a_maxgap=30;
else
  a_indold=a_ind;a_ind=a_ind+1; if (a_ind>length(a_integr)), a_ind=1; end
  a_interval=a_interval(2)+a_skip(a_indold)+[0 a_integr(a_ind)];
end
if a_integr<=0, a_interval(2)=a_end;
elseif a_interval(2)>=a_end; EOF=1; end
if a_realtime
  if d_filelist(end)<=a_interval(1)
    [EOF,jj]=update_filelist(EOF,jj);
    if EOF, return, end
  end
elseif d_filelist(end)<a_interval(1)
  EOF=1; return
end

files=d_filelist(find(d_filelist>a_interval(1) & d_filelist<=a_interval(2)));
if isempty(files) & a_integr<=0, EOF=1; return, end
i=0;
while i<length(files)
  i=i+1; file=files(i);
  filename=sprintf('%08d',file);
  i_averaged=1; i_var1=[]; i_var2=[];
  load(canon([data_path filename],0))

  lpb=length(d_parbl);
  if lpb==128
    tvec=2:4;                     % parameters holding time 
    fixed=[1 5:90 92:93 127:128]; % parameters which are not allowed to change
    az=6; el=9; fac=10;           % parameters for antenna pointing
    averaged=[6 9 96:99];         % parameters which are averaged
    accumulated=94;               % parameters which are accumulated
    ORed=95;                      % parameter which is OR'ed
    inttime=94;                   % parameter that holds integration time
    txpower=99; factx=1000;       % parameter that holds transmitter power
  else
    tvec=1:6;
    fixed=[9:10 42 64];
    az=10; el=9; fac=1;
    averaged=[8:10 42 63];
    accumulated=[7 64];
    ORed=[];
    inttime=7;
    txpower=8; factx=1;
    % do not work on unavailable parameters
    averaged(find(averaged>lpb))=[];
    fixed(find(fixed>lpb))=[];
    accumulated(find(accumulated>lpb))=[];
  end
  allow=zeros(size(fixed')); allow(find(fixed==az | fixed==el))=.11*fac;
  d_parbl=col(d_parbl);
  [secs1,year]=tosecs(d_parbl(tvec));
  a_inttime=d_parbl(inttime);
  a_ant=d_parbl([el az])/fac;
  if a_ant(1)>0
    a_elold=a_ant(1);
  elseif ~isempty(a_elold)
    a_ant(1)=a_elold;
    d_parbl(el)=a_ant(1)*fac;
  end
  a_tx=d_parbl(txpower)*factx;
  if a_tx>=0
    a_txold=a_tx;
  elseif ~isempty(a_txold)
    a_tx=a_txold;
    d_parbl(txpower)=a_tx/factx;
  end
  if a_integr<=0
    if isempty(a_antold)
      a_antold=a_ant;
      secs=secs1;
      N_averaged=0;
    end
    tdiff=secs1-secs>a_maxgap;
    if a_integr<0 & exist('starttime','var')
      tdiff=tdiff | starttime-secs1<a_integr;
    end
    adiff=a_antold-a_ant;
    a_antold=a_ant;
    if any(fix(adiff/.2)) | tdiff
      if a_ant(1)<89.8 | tdiff
        a_interval(2)=file;
        if tdiff & N_averaged>1
          a_interval(2)=file-.5; a_antold=[];
        elseif N_averaged==0
	  secs=secs1; return
        else
	  secs=secs1;
        end
        d_parbl=prev_parbl;
        break
      end
    end
  end

  secs=secs1;
  if secs<file | file-secs>=1 | year~=a_year
    disp('Filename conflicts with file contents or years do not match')
  end

  if isreal(d_data) % change to complex
    lendata=length(d_data);
    d_data(lendata-1:lendata)=abs(d_data(lendata-1:lendata));
    d_data=d_data(1:2:lendata)+sqrt(-1)*d_data(2:2:lendata);
  end

  dumpOK=(a_tx>=a_txlim);
  if dumpOK & ~isempty(a_satch)
    dumpOK=satch(a_ant(1),secs,a_inttime);
  end
  if lpb==128
    if d_parbl(95)~=0, fprintf(' Status word is %g\n',d_parbl(95)), end 
    dumpOK=dumpOK & rem(d_parbl(95),2)==0 & d_parbl(95)~=64;
  end
  if dumpOK & OK
    indfixed=fixed(find(abs(d_parbl(fixed)-prev_parbl(fixed))>allow));
    if ~isempty(indfixed)
      fprintf('Parameter change:')
      fprintf(' par(%d)=%g[%g];',[indfixed;d_parbl(indfixed)';prev_parbl(indfixed)'])
      fprintf('\n')
      if a_intfixed
       fprintf('analysis_intfixed set: ')
       if N_averaged==1
        fprintf('Skipping previous dump\n')
        OK=0; N_averaged=0;
       else
        fprintf('Skipping dump\n')
        dumpOK=0;
       end
      end
    end
  end
  if dumpOK
    if ~OK  % initialize with the first good dump
      aver=0; status=0; data=0; d_var1=0; d_var2=0; accum=0;
      starttime=secs-a_inttime;    % calculate starttime of first dump
      OK=1;
    end
    % update with the following files
    status=bitwiseor(status,d_parbl(ORed),16);
    accum=accum+d_parbl(accumulated);
    data=data+d_data;
    aver=aver+d_parbl(averaged)*i_averaged;
    if isempty(i_var1)
      d_var1=d_var1+d_data.*d_data;
      d_var2=d_var2+d_data.*conj(d_data);
    else
      d_var1=d_var1+i_var1(:);
      d_var2=d_var2+i_var2(:);
    end
    N_averaged=N_averaged+i_averaged;
    prev_parbl=d_parbl; % update previous parameter block
  elseif OK
    d_parbl=prev_parbl;
  end
  if a_realtime & file==d_filelist(end)
    [EOF,jj]=update_filelist(EOF,jj);
    if EOF, break, end
    files=d_filelist(find(d_filelist>a_interval(1) & d_filelist<=a_interval(2)));
  end
end

if OK, % if at least one good data dump was found
  % update parameter block, accept the last parameter block as starting point
  d_parbl(averaged)=aver/N_averaged;
  d_parbl(accumulated)=accum;
  d_parbl(ORed)=status;
  d_parbl(inttime)=tosecs(d_parbl(tvec))-starttime;
  d_data=data;
  if ~isempty(d_saveintdir)
   if N_averaged>1
    i_averaged=N_averaged; i_var1=real(d_var1); i_var2=d_var2;
   else
    i_averaged=[];
   end
   file=fullfile(d_saveintdir,sprintf('%08d.mat',fix(secs)));
   save_noglobal(file,d_ExpInfo,d_parbl,d_data,d_raw,i_var1,i_var2,i_averaged)
  end
  d_var1=d_var1-data.*data/N_averaged;
  d_var2=d_var2-data.*conj(data)/N_averaged;
end

function [EOF,jj]=update_filelist(EOF,jj)
global di_figures data_path b d_filelist
j=0;
a_max_rtwait=300; mrw=round(sqrt(a_max_rtwait));
if ~jj
  if di_figures(5)
    try, vizu rtgup; catch, rethrow(lasterror), end
  end
  jj=1; send_www
end

df=getfilelist(data_path,d_filelist(end));
while isempty(df)
  fprintf('.'); pause(mrw)
  [df,msg]=getfilelist(data_path,d_filelist(end));
  j=j+1;
  if j>mrw
    msg=sprintf('No new data in %ds',a_max_rtwait);
  elseif ~isempty(b) & ishandle(b(7)) & ~get(b(7),'value')
    msg='Aborting RT';
  end
  if ~isempty(msg)
    fprintf('\n%s',msg)
    EOF=1; break
  end
end
d_filelist=[d_filelist df];
if j>0, fprintf('\n'); end
