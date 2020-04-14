function r=setuplibs(unload)
global local
res=0;
libs={'libguisdap','libiri','libmsis','plwin','alt_decoder','clutter','onera_desp_lib'};
if nargin==0
 global path_GUP
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
    end
   end
  end
 end
else
 for i=1:length(libs)
  cl=char(libs(i));
  if libisloaded(cl)
   unloadlibrary(cl);
   res=res+1;
  end
 end
end
if nargout, r=res; end
