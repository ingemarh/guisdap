function [altitude,ne,te,ti,coll,cO,cM2,cH]=ionomodel(heights)
global ionomodel_first path_GUP
file=canon(fullfile(path_GUP,'matfiles','summer1996'),0);
load(file)
if isempty(ionomodel_first)
 fprintf('\n** This ionomodel uses the GIVEME model results for Summer 1996**\n')
 ionomodel_first=1;
end
ne=n_electr; 
te=t_electr;
cM2=(n_NO_ion+n_02_ion+n_N2_ion)./n_electr;
cH=n_H__ion./n_electr;
cO=1-(cM2+cH);
ti=cO.*t_O__ion+cH.*t_H__ion+cM2.*t_M2_ion;
coll=max(3578*(exp(-(altitude-100)/5.8)),10);
