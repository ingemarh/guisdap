function [h,raw]=syisr_bin(type,file,hdx)
%function [h,raw]=syisr_bin(type,file,hdx)
% Input: type flist|guess|dump|disp
global path_GUP
insert(py.sys.path,int32(0),fullfile(path_GUP,'pygup'))
getbin=py.importlib.import_module('syisrc');
py.importlib.reload(py.importlib.import_module('syisrc'));
%file='/media/mop/datamop/syisr/bin/20250822/SY_2025_08_19_22_05_11.dat1-1-134';

if strcmp(type,'flist')
 raw=[];
 flist=cell(getbin.flist(file));
 tai=num2cell(timeconv(double(flist{1})','unx2tai'));
 code=num2cell(uint16(flist{2}));
 azel=num2cell(complex(single(flist{3}),single(flist{4})));
 hdx=num2cell(uint32(flist{5}));
 nd=num2cell(uint16(flist{6}));
 h=repmat(struct('fname',file,'tai',0,'code',0,'azel',0,'hdx',0,'nd',0),[length(tai) 1]);
 [h.tai]=tai{:};
 [h.code]=code{:};
 [h.azel]=azel{:};
 [h.hdx]=hdx{:};
 [h.nd]=nd{:};
elseif strcmp(type,'guess')
 raw=[];
 h=py2mat(struct(getbin.guess(file)));
elseif strcmp(type,'dump')
 dump=cell(getbin.gethdfmat(file,uint32(hdx)));
 h=struct(dump{1});
 h.dt=uint64(h.dt); h.ipp=uint16(h.ipp); h.ws=uint16(h.ws); h.ex=char(h.ex); h.st=char(h.st);
 iq=int16(dump{2}); raw=complex(iq(2:2:end),iq(1:2:end));
elseif strcmp(type,'disp')
 head=cell(getbin.getheadmat(file));
 h=struct(head{1}); raw=struct(head{2});
 h=py2mat(h);
 raw=py2mat(raw);
else
 error('No such type')
end

function s=py2mat(s)
for field=fieldnames(s)'
 f=field{1};
 if strcmp(f(1:3),'Pad')
  s=rmfield(s,f);
 else
  v=s.(f);
  if isa(v,'py.int')
   s.(f)=int32(v);
  elseif isa(v,'py.str')
   s.(f)=char(v);
  end
 end
end
