function [dd_data]=alt_decoder(par,d_parbl,d_raw)
global path_GUP
if ~libisloaded('alt_decoder')
 libdir=fullfile(path_GUP,'lib');
 loadlibrary(fullfile(libdir,'alt_decoder.so'),fullfile(libdir,'plwin.h'))
end
nout=no_acpoints(par(3),par(5),par(4),par(1));
if par(7)<0, nout=-nout*par(2)/par(7); end
oz=zeros(nout,1);
or=libpointer('doublePtr',oz); oi=libpointer('doublePtr',oz);
up=libpointer('doublePtr',zeros(20,1)); up.value=d_parbl((1:20)+42);
calllib('alt_decoder','matface',par,length(d_raw),real(d_raw),imag(d_raw),nout,or,oi,up);
dd_data=complex(or.value,oi.value);
return

function n=no_acpoints(nsam,maxl,frac,nbit)
n=0;
for l=1:maxl
 n=n+nsam-l;
 if l>frac && rem(l,frac)~=0 && l<(nbit-1)*frac
  n=n+nsam-l;
 end
end
