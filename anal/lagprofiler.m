function lagprofiler()
global d_data d_raw d_parbl a_lpf
for lpf=a_lpf
 switch lpf.lib
 case 'plwin'
  [dd_data,upar]=plwin(lpf.par,d_parbl,d_raw(lpf.raw));
  d_parbl(42+(1:20))=upar;
 case 'alt_decoder'
  [dd_data]=alt_decoder(lpf.par,d_parbl,d_raw(lpf.raw));
 case 'clutter'
  [dd_data,upar,dd_raw]=clutter(lpf.par,d_parbl,d_raw(lpf.raw));
  d_parbl(42+(1:20))=upar;
  d_raw(lpf.raw)=dd_raw;
 case 'resampler' % par(1-5)=[decimation ntx calstart calmode]
  d_raw=sum(reshape(d_raw,lpf.par(1),[],lpf.nrep)); %resample everything
  dd_data=[];
  for i=1:lpf.par(4)
   dd_data=[dd_data;sum(abs(d_raw(lpf.par(3):end,i:lpf.par(4):lpf.nrep)).^2,2)]; % do the cal
  end
  dd_data=sum(abs(d_raw(lpf.par(4)+1:end,1:2:lpf.nrep)).^2);
  d_raw=[col(d_raw(1:lpf.par(2),1:lpf.par(3));col(d_raw(lpf.par(2)+1:lpf.par(3)-1,:))]; %separate the txsamples
 end
 fac=(diff(lpf.p)+1)/lpf.nrep;
 d_data(lpf.data+(1:length(dd_data)))=dd_data/fac;
end
endt=d_parbl(7)*(lpf.nrep-lpf.p(2)-1)/lpf.nrep;
d_parbl(1:6)=datevec(datenum(d_parbl(1:6))-endt/86400);
d_parbl(7)=d_parbl(7)*fac;
return
