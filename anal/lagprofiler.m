function lagprofiler()
global d_data d_raw d_parbl a_lpf
for lpf=a_lpf
 if ~isempty(lpf.raw)
  dd_raw=d_raw(lpf.raw);
 end
 switch lpf.lib
 case 'plwin'
  [dd_data,upar]=plwin(lpf.par,d_parbl,dd_raw);
  d_parbl(42+(1:20))=upar;
 case 'alt_decoder'
  [dd_data]=alt_decoder(lpf.par,d_parbl,dd_raw);
 case 'clutter'
  [dd_data,upar,dd_raw]=clutter(lpf.par,d_parbl,dd_raw);
  d_parbl(42+(1:20))=upar;
  d_raw(lpf.raw)=dd_raw;
 case 'resampler' % par(1-5)=[decimation ntx nsig ncal calmode ncodes]
  draw=reshape(d_raw,lpf.nsamp,[]); %reshape
  draw=sum(reshape(draw(1:sum(lpf.par(2:4))*lpf.par(1),:),lpf.par(1),[],lpf.nrep)); %resample everything
  tx=draw(1,1:lpf.par(2),1:lpf.par(6));
  sig=draw(1,lpf.par(2)+(1:lpf.par(3)),:);
  cal=reshape(draw(1,lpf.par(2)+lpf.par(3)+(1:lpf.par(4)),:),[],lpf.nrep);
  dd_data=[];
  for i=1:lpf.par(5)
   dd_data=[dd_data;sum(abs(cal(:,i:lpf.par(5):lpf.nrep)).^2,2)]; % do the cal
  end
  d_raw=[tx(:);sig(:)]; %separate the txsamples
 end
 fac=(diff(lpf.p)+1)/lpf.nrep;
 d_data(lpf.data+(1:length(dd_data)))=dd_data/fac;
end
endt=d_parbl(7)*(lpf.nrep-lpf.p(2)-1)/lpf.nrep;
d_parbl(1:6)=datevec(datenum(d_parbl(1:6))-endt/86400);
d_parbl(7)=d_parbl(7)*fac;
return
