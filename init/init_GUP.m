% init_GUP.m: Main program to find lag profile groups and calculate ambiguity functions
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
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

% Chdir to the experiment directory, store the present path as a return address
original_path=pwd;
fprintf('\n\nChdir to the experiment directory\n')
cd(canon(path_expr))

nat_const

if exist('N_rcprog')~=1, N_rcprog=1; end
for d_rcprog=1:N_rcprog
  Stime=clock;

  if N_rcprog>1, apustr=['_',int2str(d_rcprog)]; else apustr=[]; end
  load_GUPvar

  read_specpar 
  constants

  findlpg
  find_vcgroups
  find_calibrations
  ambcalc,    

  if all(p_XMITloc==p_RECloc) % Monostatic case
    lpgwrcalc,  
    lpgwomcalc,
  else
    disp([' Bistatic measurement:'])
%   Assume that the scattering volume is always completely filled
%   vc_Aenv=ones(600,length(vc_ch)); % For unit length
%   fir=lp_fir; lp_fir=abs(lp_fir);
%   lpgwomcalc, 
%   lp_fir=fir;vc_Aenv=zeros(size(vc_env));
%   vc_Aenv=zeros(size(vc_env));
% New for gup-1.70
    lpgwrcalc,  
    lpgwomcalc, 
  end

  plot(p_om,real(lpg_wom)), drawnow
  lpg_tex
  fprintf('  Time used in initialisation:%8.2f min\n',etime(clock,Stime)/60)
  save_toinitfile 

  fprintf('Radar controller program %g ready\n',d_rcprog) 
end
%fprintf(['\nChdir back to the original directory ', original_path,'\n'])
%cd(original_path)
fprintf('\n*****************************************************\n')
fprintf('*\n*\n* Execute plot_wr to see the range ambiguity functions\n')
fprintf('*\n*\n******************************************************\n')
clear original_path Stime d_rcprog
