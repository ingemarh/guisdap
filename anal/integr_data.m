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
function  [OK,EOF]=integr_data(txlim)
 
global d_parbl d_data d_var1 d_var2 data_path d_filelist a_control 
global a_ind a_interval a_year a_start a_integr a_skip a_end 
global a_txlim a_realtime a_inttime
 
OK=0; EOF=0; jj=0;
if a_ind==0
  a_inttime=60;
  a_ind=1;
  a_interval=a_start+[0 a_integr(1)];
  a_oldtime=0;
else
  a_indold=a_ind;a_ind=a_ind+1; if (a_ind>length(a_integr)), a_ind=1; end
  a_interval=a_interval(2)+a_skip(a_indold)+[0 a_integr(a_ind)];
end
if a_integr==0, a_interval(2)=a_end;
elseif a_interval(2)>=a_end; EOF=1; end
if a_realtime
  if d_filelist(end)<a_interval(1)
    [EOF,jj]=update_filelist(EOF,jj)
    if EOF, return, end
  end
elseif d_filelist(end)<a_interval(1)
  EOF=1; return
end

files=d_filelist(find(d_filelist>a_interval(1) & d_filelist<=a_interval(2)));
if isempty(files) & a_integr==0, EOF=1; return, end
i=0;
while i<length(files)
  i=i+1; file=files(i);
  filename=sprintf('%08d',file);
  load(canon([data_path filename],0))

  if length(d_parbl)==128
    fixed=[1 5:90 92:93 127:128]; % parameters which are not allowed to change
    az=6; el=9; fac=10;           % parameters for antenna pointing
    averaged=[6 9 96:99];         % parameters which are averaged
    ORed=95;                      % parameter which is OR'ed
    inttime=94;                   % parameter that holds integration time
    txpower=99; factx=1000;       % parameter that holds transmitter power
  else
    fixed=[9:10];
    az=9; el=10; fac=1;
    averaged=[8:10];
    ORed=[];
    inttime=7;
    txpower=8; factx=1;
  end
  allow=zeros(size(fixed')); allow(find(fixed==az | fixed==el))=.1*fac;
  d_parbl=col(d_parbl);
  if length(d_parbl)==128
    [secs1,year]=tosecs(d_parbl(2:4));
  else
    [secs1,year]=tosecs(d_parbl(1:6));
  end
  a_inttime=d_parbl(inttime);
  if a_integr==0
    if ~exist('a_antold','var')
      a_antold=d_parbl([el az])/fac;
      secs=secs1;
      N_averaged=0;
      maxmissingdumps=3;
    end
    tdiff=secs1-secs>a_inttime*maxmissingdumps;
    if any(fix((a_antold-d_parbl([el az])/fac)/.1)) | tdiff
      a_antold=d_parbl([el az])/fac;
      if a_antold(1)<89.9 | tdiff
        d_parbl=prev_parbl;
        a_interval(2)=file;
        if N_averaged>1
          a_interval(2)=file-.5;
        end
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

  if length(d_parbl)==128
    if d_parbl(95)~=0, fprintf(' Status word is %g\n',d_parbl(95)), end 
    dumpOK=rem(d_parbl(95),2)==0 & d_parbl(95)~=64;
  else
    dumpOK=(d_parbl(txpower)*factx>=a_txlim);
  end
  if ~OK & dumpOK,  % initialize with the first good dump
    first_parbl=d_parbl;         % save the first parameter block
    prev_parbl=d_parbl;          % initialize the previous parameter block
    aver=d_parbl(averaged);      % initialize averaging
    status=d_parbl(ORed);        % save the status word
    starttime=secs-d_parbl(inttime); % calculate starttime of first dump
    if (exist('pre_integrated') == 1), 
      aver=d_parbl(averaged)*i_averaged; % initialize averaging
      data=d_data;
      if exist('i_var1') % The integrated file need not have variances in it
        d_var1=i_var1(:);
        d_var2=i_var2(:);
        N_averaged=i_averaged;
      else
        d_var1=d_data.*d_data; 
        d_var2=d_data.*conj(d_data);
        N_averaged=1;
      end
    else
      aver=d_parbl(averaged);      % initialize averaging
      data=d_data;                 % data is now a complex column vector
      d_var1=d_data.*d_data;       % for the variance calculations
      d_var2=d_data.*conj(d_data);
      N_averaged=1;
    end
    OK=1;
  elseif OK & dumpOK,  % update with the following files
    if any(abs(d_parbl(fixed)-prev_parbl(fixed))>allow)
   %  warning(' change in the parameter block')
      indfixed=find(d_parbl(fixed)~=prev_parbl(fixed));
      disp('    param#  previous   current ')
      indfixed=fixed(indfixed);
      disp([indfixed',prev_parbl(indfixed),d_parbl(indfixed)]) 
    end
    status=bitwiseor(status,d_parbl(ORed),16);
    if (exist('pre_integrated')==1)
      aver=aver+d_parbl(averaged)*i_averaged;
      data=data+d_data;
      if exist('i_var1')
        d_var1=d_var1+i_var1(:);
        d_var2=d_var2+i_var2(:);
        N_averaged=N_averaged+i_averaged;
      else
        d_var1=d_var1+d_data.*d_data;
        d_var2=d_var2+d_data.*conj(d_data);
        N_averaged=N_averaged+1;
      end
    else
      aver=aver+d_parbl(averaged);
      data=data+d_data;
      d_var1=d_var1+d_data.*d_data;
      d_var2=d_var2+d_data.*conj(d_data);
      N_averaged=N_averaged+1;
    end
  end
  prev_parbl=d_parbl; % update previous parameter block
  if a_realtime & file==d_filelist(end)
    [EOF,jj]=update_filelist(EOF,jj)
    if EOF, break, end
    files=d_filelist(find(d_filelist>a_interval(1) & d_filelist<=a_interval(2)));
  end
end

if OK, % if at least one good data dump was found
  if a_control(4)==1 & N_averaged<2,
    fprintf('One dump is not enough for variance determination\n')
    fprintf('Skipping this integration period. Command ''analysis_control(4)=2''\n')
    fprintf('in the startup file will enable the analysis\n')
    OK=0;
    return
  elseif a_control(4)==1 & N_averaged<5,
    disp(sprintf('Warning: %.0f dumps may not be enough for reliable variance determination\n',N_averaged))
  end

  % update parameter block, accept the last parameter block as starting point
  d_parbl(averaged)=aver/N_averaged;     
  d_parbl(ORed)=status;
  d_parbl(inttime)=secs-starttime;
  d_data=data;
  d_var1=d_var1-data.*data/N_averaged;
  d_var2=d_var2-data.*conj(data)/N_averaged;
end

function [EOF,jj]=update_filelist(EOF,jj)
global di_figures data_path b d_filelist
j=0;
a_max_rtwait=300; mrw=round(sqrt(a_max_rtwait));
if ~jj
  if di_figures(5)
    try, vizu rtgup; catch, disp(lasterr), end
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
