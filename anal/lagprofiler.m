function lagprofiler()
global d_data d_raw d_parbl a_lagprofiling
for lpf=a_lagprofiling'
 if strcmp(lpf.lib,'plwin')
  [dd_data,upar]=plwin(lpf.par,d_parbl,d_raw(lpf.raw));
   d_parbl(42+(1:20))=upar;
 elseif strcmp(lpf.lib,'alt_decoder')
  [dd_data]=alt_decoder(lpf.par,d_parbl,d_raw(lpf.raw));
 elseif strcmp(lpf.lib,'clutter')
  [dd_data,upar,dd_raw]=clutter(lpf.par,d_parbl,d_raw(lpf.raw));
   d_parbl(42+(1:20))=upar;
   d_raw(lpf.raw)=dd_raw;
 end 
 d_data(lpf.data+(1:length(dd_data)))=dd_data;

 endt=d_parbl(7)*(lpf.nrep-lpf.p(2)-1)/lpf.nrep;
 d_parbl(1:6)=datevec(datenum(d_parbl(1:6))-endt/86400);
 d_parbl(7)=d_parbl(7)*(diff(lpf.p)+1)/lpf.nrep;
end
return
