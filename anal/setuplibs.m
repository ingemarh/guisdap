function r=setuplibs(unload)
global local path_GUP
res=0;
libs={'libguisdap','libiri','libmsis','plwin','alt_decoder','clutter','onera_desp_lib'};
mmodel=fullfile(path_GUP,'models_m');
if nargin==0
 atmos=[0 1 1 0 0 0 1]; res_atmos=0;
 heads={'libgup','iri','msis','plwin','plwin','plwin','onera_desp_lib'};
 d=fullfile(path_GUP,'lib');
 if exist(d,'dir')
  for i=1:length(libs)
   cl=char(libs(i));
   if libisloaded(cl)
    res=res+1;
   else
    cl=fullfile(d,cl);
    l=[cl '.so'];
    h=fullfile(d,[char(heads(i)) '.h']);
    if exist(l,'file') && exist(h,'file')
     loadlibrary(cl,h);
     res=res+1;
     res_atmos=res_atmos+atmos(i);
    end
   end
  end
 end
 if res_atmos~=sum(atmos)
  addpath(mmodel)
 elseif contains(mmodel,path)
  rmpath(mmodel)
 end
else
 for i=1:length(libs)
  cl=char(libs(i));
  if libisloaded(cl)
   unloadlibrary(cl);
   res=res+1;
  end
 end
 addpath(mmodel)
end
if nargout, r=res; end
