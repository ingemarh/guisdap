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

temp=[path_expr name_expr, name_site];
t_RECloc=p_RECloc;
if name_site=='K' | name_site=='S';
  temp=[path_expr name_expr 'R'];
end
if exist('d_rcprog','var'), rcp=d_rcprog; else rcp=0; end
if exist(canon([temp '_' int2str(rcp) 'GUPvar.mat'],0),'file')
  load(canon([temp '_' int2str(rcp) 'GUPvar']))
elseif exist(canon([temp 'GUPvar.mat'],0),'file')
  load(canon([temp 'GUPvar']))
else
  fprintf(['\n\n\n GUP variable file    ', canon([temp 'GUPvar.mat'],0),'    not found \n\n\n'])
  error(' ')
end
if GUP_iniver<1.52,
  fprintf('*\n*\n* Files produced by GUP version %.2f not usable\n', GUP_iniver)
  fprintf('* Please, reinitialize the experiment with GUP 1.52 or later\n*\n*\n')
  error('')
end

if max(lp_nfir)>1
  lp_fir=cumsum(full(lp_firsto));
else 
  lp_fir=lp_firsto;
end
p_RECloc=t_RECloc;
clear rcp t_RECloc
