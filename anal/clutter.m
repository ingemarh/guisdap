function [dd_data,upar,d_raw]=clutter(par,d_parbl,d_raw)
global path_GUP
if ~libisloaded('clutter')
 libdir=fullfile(path_GUP,'lib');
 loadlibrary(fullfile(libdir,'clutter.so'),fullfile(libdir,'plwin.h'))
end
nout=length(d_raw);
oz=zeros(nout,1);
or=libpointer('doublePtr',oz); oi=libpointer('doublePtr',oz);
up=libpointer('doublePtr',zeros(20,1)); up.value=d_parbl((1:20)+42);
calllib('clutter','matface',length(par),par,0,[],length(d_raw),real(d_raw),imag(d_raw),nout,or,oi,up);
dd_data=complex(or.value,oi.value);
return
