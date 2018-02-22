% integr_quijing: Integration program for QUJING data stored as binary files
% GUISDAP v8.9   16-09-01 Copyright EISCAT
%
% Integration program for radar data.
% Each call to this routine performs the integation for one integration period
%
% NOTE: This is a QUJING specific function
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
% See also: an_start integrate integr_data
% function  [OK,EOF]=integr_qujing
function  [OK,EOF,N_averaged,M_averaged]=integr_qujing
 
global d_parbl d_data d_var1 d_var2 data_path d_filelist d_raw
global d_saveint
global a_ind a_interval a_year a_start a_integr a_skip a_end 
global a_realtime a_satch a_txpower
global a_intfixed a_intallow a_intfixforce a_intfix
persistent a_max secs fileslist

OK=0; EOF=0; jj=0; N_averaged=0; M_averaged=0;
d_ExpInfo=[]; d_raw=[]; txdown=0;
if a_ind==0
  a_ind=1;
  d=cell2mat({d_filelist.time});
  d=find(d<=a_start | d>a_end);
  d_filelist(d)=[];
  a_interval=a_start+[0 a_integr(1)];
else
  a_indold=a_ind; a_ind=a_ind+1; if a_ind>length(a_integr), a_ind=1; end
  a_interval=a_interval(2)+a_skip(a_indold)+[0 a_integr(a_ind)];
end
if a_integr<=0, a_interval(2)=a_end;
elseif a_interval(2)>=a_end; EOF=1; end
if d_filelist(end).time<a_interval(1)
  EOF=1; return
end

if length(fileslist)~=length(d_filelist)
 fileslist=cell2mat({d_filelist.time});
end
files=d_filelist(find(fileslist>a_interval(1) & fileslist<=a_interval(2)));
if isempty(files) & a_integr<=0, EOF=1; return, end
i=0;
while i<length(files)
  i=i+1; file=files(i);
  filename=fullfile(file.dir,file.file);
  i_averaged=1; i_var1=[]; i_var2=[];
  try
  [d_parbl,d_data]=load_qfile(filename,file.time/86400+8/24);

  lpb=length(d_parbl);
  tvec=1:6;            % parameters holding time
  fixed=a_intfix;      % parameters which are not allowed to change
  allow=a_intallow;    % max allowed change of fixed pars
  az=[]; el=[];        % parameters for antenna pointing
  averaged=[];         % parameters which are averaged
  accumulated=[64];    % parameters which are accumulated
  inttime=7;           % parameter that holds integration time
  positive=[];         % parameters which are positive
  non_negative=[];     % parameters which are positive
  % do not work on unavailable parameters
  averaged(find(averaged>lpb))=[];
  allow(find(fixed>lpb))=[];
  fixed(find(fixed>lpb))=[];
  accumulated(find(accumulated>lpb))=[];
  positive(find(positive>lpb))=[];
  non_negative(find(non_negative>lpb))=[];

  [secs,year]=tosecs(d_parbl(tvec));
  a_inttime=d_parbl(inttime);

  if isreal(d_data) % change to complex
    lendata=length(d_data);
    d_data(lendata-1:lendata)=abs(d_data(lendata-1:lendata));
    d_data=d_data(1:2:lendata)+sqrt(-1)*d_data(2:2:lendata);
  end

  dumpOK=true;
  if ~isempty(a_satch)
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
  catch
    dumpOK=0;
    [lastmsg,lastID]=lasterr;
    disp([lastID ' in ' file.file])
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
end

if OK, % if at least one good data dump was found
  % update parameter block, accept the last parameter block as starting point
  d_parbl(averaged)=aver/M_averaged;
  d_parbl(accumulated)=accum;
  d_parbl(inttime)=tosecs(d_parbl(tvec))-starttime;
  d_data=data;
  M_averaged(1)=max(N_averaged);
  M_averaged(2)=min(N_averaged);
  if ~isempty(d_saveint)
   if all(M_averaged==1)
    i_averaged=[];
   else
    i_averaged=N_averaged; i_var1=real(d_var1); i_var2=d_var2;
   end
   file=[d_saveint.dir sprintf('%08d.mat',fix(secs))];
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
