% load_PS.m: loads in the timing diagram variables
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
%
% loads in the timing diagram variables for radar controller program number 
% 'rcprog'. If there are multiple programs for the experiment, 'Nrcprog'
% must specify the total number
%
% See also: path_expr init_EISCAT 
% function load_PS(rcprog, Nrcprog)
  function load_PS(rcprog, Nrcprog)

global path_exps name_expr name_site 
global td_ch td_t1 td_t2 td_am p_rep ch_f ch_filter ch_adcint p_offsetppd

if nargin==0, rcprog=1; Nrcprog=1; 
elseif nargin==1, Nrcprog=1; end
apustr=['_',int2str(rcprog)];

create=1;
PSfile=find_apustr_file([path_expr name_expr name_site],apustr,'pat_PS','.mat');
if isempty(PSfile)
  fprintf('Did not find a pat_PS.mat file')
else
  fprintf(' Loading %s\n',PSfile)
  load(PSfile)
  if exist('ch_f')==1 & any(td_ch==0)
    create=0;
  elseif all(td_ch~=0)   % No calibration data in the file
    fprintf('No calibration times in pat_PS.mat file')
  else
    fprintf('Existing pat_PS.mat file is not up-to-date')
  end
end

if create
  PSfile=find_apustr_file([path_expr name_expr name_site],apustr,'pat_PS','.m');
  if isempty(PSfile)
    fprintf('\n')
    error('Please make a pat_PS.m with TLAN2PS or a pat_PS.mat file yourself')
  else
    fprintf(': Creating new one\n')
  end
  cd(path_expr)
  td_len=10000;
  td_ind=0;
  td_ch=zeros(1,td_len);
  td_t1=zeros(1,td_len);
  td_t2=zeros(1,td_len);
  td_am=zeros(1,td_len);
  ch_f=-1*ones(1,8);

  fprintf('Decoding the %s.m file\n',PS_file)
  eval(PS_file) 
  if td_ind<td_len;
    td_ch(td_ind+1:td_len)=[];
    td_t1(td_ind+1:td_len)=[];
    td_t2(td_ind+1:td_len)=[];
    td_am(td_ind+1:td_len)=[];
  end
  if all(ch_f==-1) | all(td_ch~=0)
    %row(ch_f), row(td_ch) 
    error('The pat_PS.m file is not up-to-date. Create again with new version of TLAN2PS before proceeding')
  end
  save_PS
end
