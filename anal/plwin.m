function [dd_data,upar]=plwin(par,d_parbl,d_raw)
global path_GUP
if ~libisloaded('plwin')
 libdir=fullfile(path_GUP,'lib');
 loadlibrary(fullfile(libdir,'plwin.so'),fullfile(libdir,'plwin.h'))
end
nout=((par(3)+1)*par(9)+par(3)*par(2)/2+par(4)*par(9))*par(10)+(par(7)+par(11))*par(10)+par(20)*(par(18)+par(19)+1);
oz=zeros(nout,1);
or=libpointer('doublePtr',oz); oi=libpointer('doublePtr',oz);
up=libpointer('doublePtr',zeros(20,1)); up.value=d_parbl((1:20)+42);
calllib('plwin','matface',par,length(d_raw),real(d_raw),imag(d_raw),nout,or,oi,up);
dd_data=complex(or.value,oi.value);
upar=up.value;
