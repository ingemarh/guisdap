function r=setuplibs(unload)
global local path_GUP path_tmp
if nargout, r=0; end
if isempty(path_GUP), return, end
res=0;
libs={'libguisdap','libiri','libmsis','plwin','alt_decoder','clutter','cluttlp','onera_desp_lib'};
ll=length(libs);
thdir=fullfile(path_tmp,'gupthunk');
mmodel=fullfile(path_GUP,'models_m');
if nargin==0
 atmos=zeros(1,ll); atmos([2 3 end])=1; res_atmos=0;
 heads={'libgup','iri','msis','plwin','plwin','plwin','plwin','onera_desp_lib'};
 init={[],[],{'matface_' length(path_GUP)+12 fullfile(path_GUP,'share','msis',filesep)},[],[],[],[],[]};
 d=fullfile(path_GUP,'lib');
 if ~exist(thdir,'dir'), mkdir(thdir), end
 owd=pwd;
 if ispc, libext = '.dll';
 elseif ismac, libext = '.dylib';
 else libext = '.so'; end
 try, cd(thdir) % ugly, but matlab bugs
 if exist(d,'dir')
  for i=1:ll
   cl=char(libs(i));
   if libisloaded(cl)
    res=res+1;
   else
    th=fullfile(cl);
    cl=fullfile(d,cl);
    l=[cl libext];
    h=fullfile(d,[char(heads(i)) '.h']);
    if exist(l,'file') && exist(h,'file')
     try
      loadlibrary(cl,h,'thunkfilename',th)
      if ~isempty(init{i})
       ii=[char(libs(i)) init{i}];
       feval('calllib',ii{:})
      end
      res=res+1;
      res_atmos=res_atmos+atmos(i);
     catch
      disp(lasterr)
     end
    end
   end
  end
 end
 cd(owd)
 catch, cd(owd), disp(lasterr)
 end
 if res_atmos~=sum(atmos)
  addpath(mmodel)
 elseif contains(mmodel,path)
  rmpath(mmodel)
 end
else
 for i=1:ll
  cl=char(libs(i));
  if libisloaded(cl)
   unloadlibrary(cl);
   res=res+1;
  end
 end
 addpath(mmodel)
end
if nargout, r=res; end
