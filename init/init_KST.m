% init_KST.m: The main program to produce the GUISDAP variables for EISCAT experiments
% GUISDAP v1.80   02-02-27 Copyright EISCAT, Huuskonen&Lehtinen
% 
% The user files are found in the exps/CP1K directory
%     CP1KT_init.m   : definition of basic time unit etc
%     CP1KTpat_PS.m  : transmission, reception and filter definitions
%     CP1KT_vcinit.m : virtual channel definitions
%     CP1KT_LP.m     : lag profile definitions 
%
% Other files called
%  td_arrange      % various helpful things performed
%  envcalc         % forms the transmitter envelopes 
%  find_sampling   % find when virtual channels sample
%  vc_arrange      % cleaning of the virtual channel variables
%
% init_EISCAT and the routines called by it do not, strictly speaking, 
% belong to GUISDAP. . They form a method of producing GUISDAP variables 
% for EISCAT experiments. Therefore these routines are allowed to contain 
% explicit reference to EISCAT radar parameters and the site letters
%  'T', 'V', 'K', 'S' and 'R' have predefined meanings
%
% See also: design glob_EISCAT td_arrange envcalc find_sampling vc_arrange
%
clf, hold off
glob_initKST

% Chdir to the experiment directory, store the present path as a return address
original_path=pwd;
fprintf('\n\nChdir to the experiment directory\n')
cd(canon(path_expr))

% These values will be used if not redefined later
p_dtau=1;   
if name_site=='T' | name_site=='K' | name_site=='S' | name_site=='R'
  ch_gain=10^4.81*ones(1,8);
  ch_fradar=931.5e6*ones(1,8);
elseif name_site=='L'
  ch_gain=10^4.25*ones(1,4);
% ch_gain=10^4.48*ones(1,4); % esr-2?
  ch_fradar=500e6*ones(1,4);
elseif name_site=='V'
  ch_gain=10^4.31*ones(1,8); % full antenna?
% ch_gain=10^4.01*ones(1,4); % half antenna?
% ch_gain=10^4.60*ones(1,4); full antenna?
  ch_fradar=224e6*ones(1,8);
end
% For compatibility with the experiment design package define
p_ND=1;

% Put the transmitter/receiver locations to GUP variables
p_XMITloc=[69.583 19.21 .030];  % Latitude, longitude, altitude
if name_site=='T' | name_site=='V' 
  p_RECloc=[69.583 19.21 .030];
elseif name_site=='K'
  p_RECloc=[67.863 20.44 .412];
elseif name_site=='S' | name_site=='R'
  p_RECloc=[67.367 26.65 .180];
elseif name_site=='L'
  p_RECloc=[78.153 16.029 .438];
  p_XMITloc=p_RECloc;
end

% The calibration temperatures, which may be redefined during analysis
if name_site=='T' | name_site=='V' 
  p_calTemp=210;
else
  p_calTemp=30;
end

if exist('N_rcprog')~=1, N_rcprog=1; end
for d_rcprog=1:N_rcprog
  Stime=clock;

  if N_rcprog>1, apustr=['_',int2str(d_rcprog)]; else apustr=[]; end

  file=canon([name_expr name_site '_init'],0);
  for i=1:3,fprintf('\n'), end
  if exist(file)==2
    eval(file)
    fprintf(['# ',file, ' executed\n'])
  else
    fprintf(['# ',file, ' not available ...... Standard values will be used\n'])
  end
  for i=1:3,fprintf('#\n'), end

  if p_dtau<0.01;  % Due to change in p_dtau definition
     p_dtau=p_dtau*1e6;
     for i=1:10,fprintf('*********\n');end
     fprintf(' Starting from GUP 1.50 p_dtau is given in microseconds\n')
     fprintf(' change p_dtau to get rid of this message\n')
     for i=1:10,fprintf('*********\n');end
  end  
  if length(ch_fradar)==1
    ch_fradar=ch_fradar*ones(1,8);
  end
  fprintf('\n\n# Basic time unit is %.1f us\n# Frequencies are\n#',p_dtau)
  fprintf(' %.1f MHz', ch_fradar/1e6), fprintf('\n\n\n')

  global td_am td_ch td_t1 td_t2 c_f
  if name_site=='T' | name_site=='V' | name_site=='L'
    load_PS(d_rcprog,N_rcprog)
  else
    load_PSrem
  end

  fprintf('\n\n# Defining virtual channels:\n')
  file=canon([name_expr name_site 'vcinit'],0);
  fprintf('#####\n')
  if exist(file)==2
    eval(file)
    fprintf(['# ' file ' executed\n'])
  else
    fprintf(['# ' file ' not available \n'])
    fprintf('# Assuming virtual channels extending from 0 to REP (%.0f)\n',p_rep)
    vc_ch=diff_val(td_ch(td_ch~=0)); 
    vc_t1=zeros(size(vc_ch));
    vc_t2=p_rep*ones(size(vc_ch));
    vc_mf=zeros(size(vc_ch));
  end
  fprintf('### Virtual channels defined \n')

  td_arrange
  fprintf(['\n\nCalculating transmission envelopes and receiver impulse responses ...\n'])
  envcalc
  fprintf(['\n ............Calculated \n\n'])
 
  find_sampling % find when virtual channels sample
  
  clear td_am td_t1 td_t2 td_ch
  vc_arrange

  fprintf('\nDefining lag profiles by executing %s%s_LP:\n',name_expr,name_site)
  eval(canon([name_expr name_site '_LP'],0))
  fprintf(['\n%s%s_LP passed\n\n',name_expr,name_site)

  fprintf('Time used in initialization:%8.2f min\n',etime(clock,Stime)/60)
  save_GUPvar
  fprintf('Radar controller program %g ready\n',d_rcprog) 
end
%fprintf(['\nChdir back to the original directory ', original_path,'\n'])
%cd(original_path)
fprintf('\n*******************************************************\n')
fprintf('* Execute plot_td to see the timing diagram\n')
fprintf('*******************************************************\n')
clear original_path i file 
