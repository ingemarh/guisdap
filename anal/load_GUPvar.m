% load_GUPvar: A script to load GUPvariables into the workspace 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% A script to load GUPvariables into the workspace 
% The script assumed that variables name_expr and name_site exist in the workspace
% The script contains reference to EISCAT remote sites. However, the script works
% without modifications for other radars as long as name_site is different from K and S
%
% See also: path_expr save_toinitfile
%

temp=[path_expr name_expr name_site];
t_RECloc=p_RECloc;
if name_site=='K' | name_site=='S'
  temp=[path_expr name_expr 'R'];
end
if exist('d_rcprog','var')
  GUPvarfile=[temp '_' int2str(d_rcprog) 'GUPvar'];
  GUPvarfil=canon(GUPvarfile,0);
end
if ~exist(initfil,'var') | ~exist(GUPvarfil,'file')
  GUPvarfile=[temp 'GUPvar'];
  GUPvarfil=canon(GUPvarfile,0);
end
if exist(GUPvarfil,'file')
  disp(GUPvarfile)
  load(GUPvarfil)
else
  error(['GUP variable file ' GUPvarfile '.mat not found'])
end
if GUP_iniver<1.52
  error(sprintf('Files produced by GUP vs %.2f not usable: Reinitialise!',GUP_iniver))
end

if max(lp_nfir)>1
  lp_fir=cumsum(full(lp_firsto));
else 
  lp_fir=lp_firsto;
end
p_RECloc=t_RECloc;
clear t_RECloc GUPvarfile GUPvarfil
