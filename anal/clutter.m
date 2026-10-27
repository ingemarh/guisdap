function [dd_data,upar,dd_raw]=clutter(par,d_parbl,d_raw)
no=par(3)*par(8)-(par(8)-1)*par(8)/2;
if par(8)==0, no=1; end
oz=zeros(no,1);
or=libpointer('doublePtr',oz); oi=libpointer('doublePtr',oz);
%up=libpointer('doublePtr',zeros(20,1)); up.value=d_parbl((1:20)+42);
up=libpointer('doublePtr',d_parbl((1:20)+42));
nr=numel(d_raw);
if isa(d_raw,'int16')
 ir=libpointer('int16Ptr',real(d_raw)); ii=libpointer('int16Ptr',imag(d_raw));
 lib='clutter';
elseif isa(d_raw,'single')
 ir=libpointer('singlePtr',real(d_raw)); ii=libpointer('singlePtr',imag(d_raw));
 lib='clutter_f';
else
 error(sprintf('d_raw of undefined size: %s',class(d_raw)))
end
if ~libisloaded(lib)
 setuplibs
end
calllib(lib,'matface',par,nr,ir,ii,no,or,oi,up);
if no>1
 dd_data=complex(or.value,oi.value);
 max(imag(dd_data(1:par(3))));
else
 dd_data=[];
end
dd_raw=complex(ir.value,ii.value);
upar=up.value;
return
