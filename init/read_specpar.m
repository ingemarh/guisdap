% read_specpar.m: this script loads the _specpar file for the experiment
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% If the file is not found, the scale parameters will be those given in this routine
%
% See also: init_GUP

p_T0=300;
p_N0=1e11;
p_m0=[30.5 16];
 
% p_om=(-6:.1:6)'; % This range is not wide enough
p_om=2*sinh(-3:0.05:3.001)'; % Positive values shown below
%  0.00  0.10  0.20  0.30  0.40  0.51  0.61  0.71  0.82  0.93  1.04  1.16  1.27
%  1.39  1.52  1.64  1.78  1.91  2.05  2.20  2.35  2.51  2.67  2.84  3.02  3.20
%  3.40  3.60  3.81  4.03  4.26  4.50  4.75  5.01  5.29  5.58  5.88  6.20  6.54
%  6.89  7.25  7.64  8.04  8.47  8.91  9.38  9.87 10.39 10.93 11.50 12.10 12.73
% 13.39 14.08 14.81 15.58 16.38 17.23 18.12 19.05 20.04
p_R0=1000;

if exist('name_expr')==1,

  file=canon([name_expr name_site '_specpar'],0);

  if exist(file)==2, 
     eval(file), 
  else
    fprintf(['\n    ',file,' file is not available (need not be!)\n'])
    fprintf('    Hard coded values for scale parameters will be used\n')
  end
end
fprintf('Temperature scale is %.0f K\n', p_T0)
fprintf('Electron density scale is %.1e m^-3\n', p_N0)
fprintf('Ion masses are [%s] u\n', num2str(p_m0,3))
fprintf('Reference range is %.0f us\n', p_R0)
fprintf('Frequency values in scaled units range from %.1f to %.1f\n',min(p_om),max(p_om))

clear file
