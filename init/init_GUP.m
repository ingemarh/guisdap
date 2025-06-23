% init_GUP.m: Main program to find lag profile groups and calculate ambiguity functions
% GUISDAP v.8.1 06-01-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% Main program to find lag profile groups and calculate ambiguity functions
% Script and functions called:
% load_GUPvar : load GUISDAP experiment definition variables from a file
% read_specpar: defines the scale parameters and spectral grid
% findlpg     : lag profile groups are formed here
% find_vcgroups : similar virtual channels are grouped together
% find_calibrations : associates lpg's with calibration and background lpg
% ambcalc     : vc_Aenv vc_Ap vc_Apenv matrices formed
% lpgwrcalc   : calculation of the range ambiguity functions
% lpgwomcalc  : calculation of the reduced spectral ambiguity functions
% save_toinitfile : saves the variables to a file
%
% See also: load_GUPvar read_specpar nat_const constants lpg_tex save_toinitfile
%
% See also: findlpg find_vcgroups find_calibrations ambcalc lpgwrcalc lpgwomcalc

clf, hold off
glob_GUP

fprintf('\nChdir to the experiment directory: %s\n',path_expr)
cd(path_expr)

nat_const

if exist('N_rcprog')~=1, N_rcprog=1; end
if exist('B_rcprog')~=1, B_rcprog=1; end
for d_rcprog=B_rcprog:N_rcprog
  Stime=clock;

  apustr=['_' num2str(d_rcprog)];
  load_GUPvar

  read_specpar
  constants

  findlpg
  find_vcgroups
  find_calibrations
  ambcalc

  lpgwrcalc
  lpgwomcalc

  plot(p_om,real(lpg_wom)), drawnow
  %lpg_tex
  apustr=['_' num2str(d_rcprog)];
  if isempty(strfind(GUPvarfile,apustr)), apustr=''; end
  save_toinitfile

  fprintf('Time used in initialisation:%8.2f min\n',etime(clock,Stime)/60)
  fprintf('Radar controller program %g ready\n',d_rcprog)
end
fprintf('\n*****************************************************\n')
fprintf('*\n*\n* Execute plot_wr to see the range ambiguity functions\n')
fprintf('*\n*\n******************************************************\n')
clear Stime d_rcprog
