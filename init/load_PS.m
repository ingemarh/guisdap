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
if Nrcprog>1, apustr=['_',int2str(rcprog)]; else apustr=[]; end

create=1;
file=canon([ path_expr name_expr name_site apustr 'pat_PS.mat']);
if exist(file)==2
  fprintf(' Loading existing pat_PS.mat from file\n')
  load(file)
  if exist('ch_f')==1 & any(td_ch==0),
    return
  elseif all(td_ch~=0)   % No calibration data in the file
    fprintf(' No calibration times in the file: Creating new one\n')
  else
    fprintf(' Existing pat_PS.mat file is not up-to-date: Creating new one\n')
  end
end

if create
  cd(path_expr)
  td_len=10000;
  td_ind=0;
  td_ch=zeros(1,td_len);
  td_t1=zeros(1,td_len);
  td_t2=zeros(1,td_len);
  td_am=zeros(1,td_len);
  ch_f=-1*ones(1,8);

  fprintf(['\n\n Decoding the ',name_expr ,name_site, apustr, 'pat_PS file\n'])
  fprintf(' This may take some time for alternating code experiments\n')
  fprintf(' But you only have to do this once ...\n\n')

  eval(canon([name_expr name_site apustr 'pat_PS'],0)) 
  fprintf(['\n' name_expr name_site apustr 'pat_PS passed\n'])

  if td_ind<td_len;
    td_ch(td_ind+1:td_len)=[];
    td_t1(td_ind+1:td_len)=[];
    td_t2(td_ind+1:td_len)=[];
    td_am(td_ind+1:td_len)=[];
  end
  if all(ch_f==-1) | all(td_ch~=0)
    row(ch_f), row(td_ch) 
    fprintf(' The pat_PS.m file is not up-to-date\n')
    fprintf(' Create again with new version of TLAN2PS before proceeding\n')
    fprintf(' Contact a GUISPERT or GUIZARD if needed\n'),error(' ')
  end

  save(canon(file),'td_ch','td_t1','td_t2','td_am','p_rep','ch_f','ch_filter','ch_adcint');
end
