function [altitude,n_electr,t_electr,ti,coll,cO,cM2,cH]=ionomodel(heights,minfo)
if minfo
 fprintf('** The model uses the GIVEME model for Summer 1996**\n')
end
global path_GUP
file=canon(fullfile(path_GUP,'matfiles','summer1996'),0);
load(file)
cM2=(n_NO_ion+n_02_ion+n_N2_ion)./n_electr;
cH=n_H__ion./n_electr;
cO=1-(cM2+cH);
ti=cO.*t_O__ion+cH.*t_H__ion+cM2.*t_M2_ion;
coll=max(3578*(exp(-(altitude-100)/5.8)),10); % from gup150 model
