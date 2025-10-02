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
function  [OK,EOF,N_averaged,M_averaged]=integr_syisr()
 
global d_parbl d_data d_var1 d_var2 data_path d_filelist d_raw
global d_saveint
global a_ind a_interval a_year a_start a_integr a_skip a_end 
global a_satch a_code a_intfix a_lpf any_start a_rcprog a_loop a_screen
persistent secs a_nnold fileslist filescode filesbeam beams a_beam lraw alpf ncode

OK=0; EOF=0; N_averaged=0; M_averaged=0;
d_ExpInfo=[]; d_raw=[];

if ~isempty(a_ind) & a_ind(1)==0
  lraw=0;
  for lpf=a_lpf
    if lraw<max(lpf.raw), lraw=max(lpf.raw); end
  end
  if isfield(d_filelist,'nd')
    bad=find(cell2mat({d_filelist.nd})<lraw/a_lpf(1).nrep);
    d_filelist(bad)=[];
  end
  fileslist=cell2mat({d_filelist.tai});
  filescode=cell2mat({d_filelist.code});
  filesbeam=cell2mat({d_filelist.azel});
  if ~isempty(any_start)
    a_start=fileslist(find(a_code(1)==filescode,1));
  end
  a_interval=a_start+[0 a_integr];
  beams=unique(filesbeam);
  if ~isempty(a_screen)
    d=beams-a_screen(1); d=abs([real(d);imag(d)]);
    beams(find(d(1,:)>real(a_screen(2)) | d(2,:)>imag(a_screen(2))))=[];
  end
  a_beam=1;
  ncode=length(a_code);
  a_loop=a_lpf(1).nrep/ncode;
  alpf=a_lpf;
elseif a_integr==0
  a_interval=a_interval(2)+[0 a_integr];
else
  a_beam=a_beam+1;
  if a_beam>length(beams)
    a_beam=1;
    a_interval=a_interval(2)+[0 a_integr];
  end
end

if a_integr==0
  a_ind=find(a_interval(1)<=fileslist & a_code(1)==filescode);
  if ~isempty(a_screen)
    a_ind(1:find(ismember(filesbeam(a_ind),beams),1)-1)=[];
  end
  nextmove=find(filesbeam(a_ind)~=filesbeam(a_ind(1)),1);
  if isempty(nextmove)
    EOF=1;
  else
    a_interval(2)=fileslist(a_ind(nextmove));
    a_ind(nextmove:end)=[];
  end
else
  a_ind=find(a_interval(1)<=fileslist & a_interval(2)>fileslist & a_code(1)==filescode & beams(a_beam)==filesbeam);
end

if a_interval(1)>a_end || a_interval(1)>fileslist(end)
  EOF=1; return
end
loop=a_lpf(1).loop; laind=length(a_ind);

if ~laind, return; end
ll=laind/ceil(laind/loop);
t408=0; skys=[];
for aind=0:ll:laind-1;
  dumpOK=1;
  innerloop=fix([aind aind+ll-1]);
  loop=diff(innerloop)+1;
  if loop~=a_loop
   a_lpf=alpf; lraw=0;
   chk_lagprofiling(loop)
   for lpf=a_lpf
    if lraw<max(lpf.raw), lraw=max(lpf.raw); end
   end
   a_loop=loop;
  end
  d_raw=int16(zeros(lraw,1)); draw=[];
  for lop=0:loop-1
   fid=a_ind(innerloop(1)+lop+1);
   for j=a_code
    jj=find(filescode(fid:end)==j,1);
    if isempty(jj)
     dump_OK=0; break
    end
    file=d_filelist(fid+jj-1);
    if isempty(draw), tfirst=file.tai; end
    if isfield(file,'nd')
     [head,draw]=syisr_bin('dump',file.fname,file.hdx);
     head.nd=file.nd;
     site=head.st;
     skys=[skys;fid+jj-1];
    else
     head=h5read(file.fname,'/head',file.hdx,1);
     d_rx=h5read(file.fname,'/data',double(head.di+1),double(head.nd));
     t408=t408+head.tsky;
     draw=complex(double(d_rx.r),double(d_rx.i));
     draw=complex(d_rx.r,d_rx.i);
     site=file.fname(end-4); %hui hui
    end
    for lpf=a_lpf
     len_prof=length(lpf.raw)/ncode/loop;
     if head.nd<len_prof+lpf.skip
      error('guisdap:integr_data',sprintf('Need %d data points, got %d',len_prof+lpf.skip,head.nd))
     end
     d_raw(lpf.raw((j-a_code(1)+lop*ncode)*len_prof+(1:len_prof)))=draw(lpf.skip+(1:len_prof));
    end
   end
  end
  if dumpOK
   i_averaged=1; i_var1=[]; i_var2=[];
   if isfield(head,'ex')
    d_ExpInfo=['syisr3 ' head.ex];
    rf=double(head.rf);
   elseif head.pw==head.bw
    d_ExpInfo=sprintf('syisr3 sy%d',head.bw);
    rf=440e6;
   else
    d_ExpInfo=sprintf('syisr3 sy%dx%d',head.pw/head.bw,head.bw);
   end
   tvec=1:6;          % parameters holding time
   az=10; el=9;       % parameters for antenna pointing
   averaged=[59];     % parameters which are averaged (t408)
   accumulated=[7];   % parameters which are accumulated
   inttime=7;         % parameter that holds integration time
   non_negative=[9];  % parameters which are positive
   secs=double(head.ipp)*1e-6+file.tai;
   d_parbl(inttime)=secs-tfirst;
   d_parbl(tvec)=timeconv(secs,'tai2utc');
   d_parbl(8)=4.7e6; % dummy hui hui
   d_parbl([az el])=[real(file.azel) imag(file.azel)];
   d_parbl(41)=9+strfind('SWD',site);
   d_parbl(43:62)=0; % to clear upars
   d_parbl(57)=a_rcprog;
   d_parbl(58:62)=[rf;0;double(head.ws);head.et;head.at];

   d_data=NaN(0,1);
%  figure(9),subplot(3,1,1),plot(real(d_raw))
   lagprofiler()
%  figure(9),subplot(3,1,2),plot(real(d_raw))
%  figure(9),subplot(3,1,3),plot(real(d_data)),drawnow
 
   a_inttime=d_parbl(inttime);
   d=find(d_parbl(non_negative)<=0);
   if ~isempty(d)
    d_parbl(non_negative(d))=a_nnold(d);
   end
   a_nnold=d_parbl(non_negative);
 
   if ~isempty(a_satch)
    %if ~isfield(a_satch,'prep'), a_satch.prep=a_inttime*1e6; end
    [dumpOK,ad_sat]=satch(d_parbl(el),secs,a_inttime);
   end
  end
  if dumpOK
    if ~OK  % initialize with the first good dump
      data=zeros(size(d_data)); d_var1=data; d_var2=data; accum=0;
      starttime=secs-a_inttime;    % calculate starttime of first dump
      OK=1;
    end
    % update with the following files
    accum=accum+d_parbl(accumulated);
    %aver=aver+d_parbl(averaged);
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

if OK, % if at least one good data dump was found
  % update parameter block, accept the last parameter block as starting point
  if ~isempty(skys)
    if isfield(file,'t408')
     t408=sum(cell2mat({d_filelist(skys).t408}));
    else
     tu=round(timeconv(fileslist(skys),'tai2unx')/3)*3; [~,ia,ib]=unique(tu); %3sec resolution
     t408u=skytemp(site,tu(ia),real(filesbeam(skys(ia))),imag(filesbeam(skys(ia))));
     t408=sum(t408u(ib));
    end
  end
  d_parbl(accumulated)=accum;
  d_parbl(averaged)=t408/laind/ncode;
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
   file=[d_saveint.dir sprintf('%012.3f.mat',d(2))];
   disp(file)
   if d_saveint.var
    save_noglobal(file,d_ExpInfo,d_parbl,d_data,i_var1,i_var2,i_averaged)
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
