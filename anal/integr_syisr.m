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
global a_lpf a_code
persistent a_max secs a_nnold a_averold a_beam d_dir d_ran

OK=0; EOF=0; N_averaged=0; M_averaged=0;
d_ExpInfo=[]; d_raw=[];

code_id=a_code(1);
no_codes=length(a_code);
n_beam=size(d_filelist.code,1);

if isempty(a_beam)
 a_interval(2)=a_start;
 a_beam=0;
 d_dir=h5read(d_filelist.fname{d_filelist.fno(1)},'/Tx/Beams');
 d_ran=length(h5read(d_filelist.fname{d_filelist.fno(1)},'/Rx/RangeWindow'));
end
a_beam=a_beam+1;
if a_beam>n_beam
 a_beam=1;
 a_interval=a_interval(2)+[0 a_integr];
end
a_ind=find(a_interval(1)<=d_filelist.tai(1,:) & a_interval(2)>d_filelist.tai(1,:) & code_id==d_filelist.code(1,:));
if a_interval(1)>a_end || a_interval(1)>d_filelist.tai(1,end)
  EOF=1; return
end

i=0;
d_data=zeros(0,1);
for fid=a_ind
  i=i+1;
  i_averaged=1; i_var1=[]; i_var2=[]; d_raw=[];
  for j=-1:no_codes-2
    d_filelist.fname{d_filelist.fno(fid)};
    d_rx=int16(round(h5read(d_filelist.fname{d_filelist.fno(fid)},'/Rx/DataWindow',[1,1,a_beam,fid+j],[2,d_ran,1,1])));
    d_raw=[d_raw;transpose(complex(d_rx(1,:,1,1),d_rx(2,:,1,1)))];
  end
  d_ExpInfo=sprintf('kst0 syisr%d',code_id);
  tvec=1:6;               % parameters holding time
  az=10; el=9;            % parameters for antenna pointing
  averaged=[8:10];        % parameters which are averaged
  accumulated=[7];     % parameters which are accumulated
  inttime=7;              % parameter that holds integration time
  positive=[8];           % parameters which are positive
  non_negative=[9];       % parameters which are positive
  d_parbl(tvec)=timeconv(d_filelist.tai(a_beam,fid)+0.016,'tai2utc');
  d_parbl(inttime)=0.016;
  d_parbl(8)=2e6;
  d_parbl([az el])=d_dir(:,a_beam);
  d_parbl(41)=10;
  d_parbl(62)=0;

  lagprofiler()
 
  secs1=timeconv(row(d_parbl(tvec)),'utc2tai');
  a_inttime=d_parbl(inttime);
  d=find(d_parbl(non_negative)<=0);
  if ~isempty(d)
   d_parbl(non_negative(d))=a_nnold(d);
  end
  a_nnold=d_parbl(non_negative);
  d=d_parbl(averaged);
  % At EISCAT we sample some parameters at the end of dump
  tdiff=(round(secs1/a_inttime)-round(secs/a_inttime))*a_inttime;
  if tdiff==a_inttime
    d_parbl(averaged)=(d+a_averold)/2;
  end
  a_averold=d;

  secs=secs1;

  dumpOK=1;
  if ~isempty(a_satch)
    [dumpOK,ad_sat]=satch(d_parbl(el),secs,a_inttime);
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
