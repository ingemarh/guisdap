% load_initfile.m: loads the ambiguity functions etc. into the workspace for analysis
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The script assumed that variables name_expr and name_site exist in the workspace
% The script contains reference to EISCAT remote sites. However, the script works
% without modifications for other radars as long as name_site is different from K and S
%
% See also: path_expr save_toinitfile 
%
clear vcg_Aenv  

if ~isempty(p_RECloc), t_RECloc=p_RECloc; end
temp=[path_expr name_expr name_site];
if name_site=='K' | name_site=='S';
  temp=[path_expr name_expr 'R'];
end
initfil=tempname;
if exist('d_rcprog','var')
  initfile=[temp '_' int2str(d_rcprog) 'init'];
  initfil=canon(initfile,0);
end  
if ~exist(initfil,'file')
  initfile=[temp 'init'];
  initfil=canon(initfile,0);
end
if exist(initfil,'file')
  disp(initfile)
  load(initfil)
else
  error(['File ' initfile '.mat not found'])
end
if GUP_iniver<1.52
  error(sprintf('Files produced by GUP vs %.2f not usable: Reinitialise!',GUP_iniver))
end

if exist('vcg_Aenv')
  vc_Aenv=vcg_Aenv(:,vc_group);
  vc_Ap=vcg_Ap(:,vc_group);
  vc_Apenv=vcg_Apenv(:,vc_group);
  vc_penv=vcg_penv(:,vc_group);
  vc_penvabs=vcg_penvabs(:,vc_group);
  clear vcg_Aenv vcg_Ap vcg_Apenv vcg_penv vcg_penvabs
end
if exist('t_RECloc'), p_RECloc=t_RECloc; clear t_RECloc, end
clear temp initfile initfil
