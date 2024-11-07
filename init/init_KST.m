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
fprintf('\n\nChdir to the experiment directory: %s\n',path_expr)
cd(path_expr)

% These values will be used if not redefined later
p_dtau=1;
if name_site=='T' | name_site=='K' | name_site=='S' | name_site=='R'
  ch_gain=10^4.81;
  ch_fradar=927e6;
elseif name_site=='L' | name_site=='P'
  ch_gain=10^4.25;
% ch_gain=10^4.48; % esr-2?
  ch_fradar=500e6;
elseif name_site=='V'
  ch_gain=10^4.31; % full antenna?
% ch_gain=10^4.01; % half antenna?
% ch_gain=10^4.60; full antenna?
  ch_fradar=224e6;
elseif name_site=='Q'
  ch_gain=10^4.1;
  ch_fradar=500e6;
elseif name_site=='H'
  %ch_gain=10^4.3; %phase1
  ch_gain=10^4.6; %phase2
  ch_fradar=440e6;
elseif name_site=='D'
  ch_gain=10^4.4;
  ch_fradar=440e6;
elseif name_site=='W'
  ch_gain=10^4.4;
  ch_fradar=440e6;
end
% For compatibility with the experiment design package define
p_ND=1;

% Put the transmitter/receiver locations to GUP variables
p_XMITloc=[69.583 19.21 .030];  % Latitude, longitude, altitude
if name_site=='T' | name_site=='V' 
  p_RECloc=[69.583 19.21 .030];
  %p_RECloc=[69.586453 19.227428 0.0865];
elseif name_site=='K'
  p_RECloc=[67.863 20.44 .412];
  %p_RECloc=[67.860658 20.435222 0.4179];
elseif name_site=='S' | name_site=='R'
  p_RECloc=[67.367 26.65 .180];
  %p_RECloc=[67.363683 26.627081 0.1973];
elseif name_site=='L' | name_site=='P'
  p_RECloc=[78.153 16.029 .438];
  p_XMITloc=p_RECloc;
  %p_RECloc=[78.15313 16.02875 0.445];
elseif name_site=='Q'
  p_RECloc=[25.6381,103.7151,2.0448];
  p_XMITloc=p_RECloc;
elseif name_site=='H'
  p_RECloc=[18.349,109.622,0.055];
  p_XMITloc=p_RECloc;
elseif name_site=='D'
  p_RECloc=[19.5,109.1,0.1]; %Danzho
elseif name_site=='W'
  p_RECloc=[19.6,110.8,0.1]; %Wenchang
end

% The calibration temperatures, which may be redefined during analysis
if name_site=='T' | name_site=='V' 
  p_calTemp=210;
else
  p_calTemp=30;
end

if exist('N_rcprog')~=1, N_rcprog=1; end
if exist('B_rcprog')~=1, B_rcprog=1; end
for d_rcprog=B_rcprog:N_rcprog
  Stime=clock;

  file=find_apustr_file([name_expr name_site],d_rcprog,'_init','m');
  for i=1:3,fprintf('\n'), end
  if isempty(file)
    disp('#Standard values will be used')
  else
    eval(file)
    fprintf(['# ',file, ' executed\n'])
  end
  if p_dtau<0.01;  % Due to change in p_dtau definition
     p_dtau=p_dtau*1e6;
     for i=1:10,fprintf('*********\n');end
     fprintf(' Starting from GUP 1.50 p_dtau is given in microseconds\n')
     fprintf(' change p_dtau to get rid of this message\n')
     for i=1:10,fprintf('*********\n');end
  end

  global td_am td_ch td_t1 td_t2 c_f
  if name_site=='T' | name_site=='V' | name_site=='L' | name_site=='P' | name_site=='Q' | name_site=='H'
    load_PS(d_rcprog,N_rcprog)
  else
    load_PSrem
  end
  if length(ch_f)~=length(ch_fradar)
    ch_fradar=ch_fradar*ones(size(ch_f));
    ch_gain=10^4.81*ones(size(ch_f));
  end
  fprintf('# Basic time unit is %g us\n# Frequencies are',p_dtau)
  fprintf(' %g', ch_fradar/1e6), fprintf(' MHz\n')

  fprintf('# Defining virtual channels:\n#####\n')
  file=find_apustr_file([name_expr name_site],d_rcprog,'vcinit','m');
  if ~isempty(file)
    eval(file)
    fprintf(['# ' file ' executed\n'])
  else
    fprintf('# Assuming virtual channels extending from 0 to REP (%.0f)\n',p_rep)
    vc_ch=diff_val(td_ch(td_ch~=0)); 
    vc_t1=zeros(size(vc_ch));
    vc_t2=p_rep*ones(size(vc_ch));
    vc_mf=zeros(size(vc_ch));
  end
  fprintf('### Virtual channels defined \n')

  td_arrange
  fprintf(['Calculating transmission envelopes and receiver impulse responses ...\n'])
  envcalc
  fprintf(['............Calculated \n'])
 
  find_sampling % find when virtual channels sample
  
  clear td_am td_t1 td_t2 td_ch
  vc_arrange

  file=find_apustr_file([name_expr name_site],d_rcprog,'_LP','m');
  if isempty(file)
    error('Stopping')
  else
    fprintf('Defining lag profiles by executing %s:\n',file)
    eval(file)
    fprintf('...%s passed\n',file)
  end

  fprintf('Time used in initialization:%8.2f min\n',etime(clock,Stime)/60)
  if d_rcprog~=1
    apustr=['_' num2str(d_rcprog)];
  end
  save_GUPvar
  fprintf('Radar controller program %g ready\n',d_rcprog) 
end
%fprintf(['\nChdir back to the original directory ', original_path,'\n'])
%cd(original_path)
fprintf('\n*******************************************************\n')
fprintf('* Execute plot_td to see the timing diagram\n')
fprintf('*******************************************************\n')
clear original_path i file 
