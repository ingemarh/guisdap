% read_specpar.m: this script loads the _specpar file for the experiment
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% If the file is not found, the scale parameters will be those given in this routine
%
% See also: init_GUP

p_T0=500;
p_N0=1e11;
p_m0=[30.5 16];
 
% p_om=(-6:.1:6)'; % This range is not wide enough
p_om=sinh(-4:0.04:4.001)'; %
p_R0=1000;

if exist('name_expr')==1
  file=find_apustr_file([name_expr name_site],d_rcprog,'_specpar','m');
  if isempty(file)
    fprintf('    Hard coded values for scale parameters will be used\n')
  else
    eval(file)
  end
end
fprintf('Temperature scale is %g K\n', p_T0)
fprintf('Electron density scale is %.1e m^-3\n', p_N0)
fprintf('Ion masses are [%s] u\n', num2str(p_m0,3))
fprintf('Reference range is %g us\n', p_R0)
fprintf('Frequency values in scaled units range from %g to %g\n',min(p_om),max(p_om))

clear file
