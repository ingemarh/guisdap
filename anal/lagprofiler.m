function lagprofiler()
global d_data d_raw d_parbl a_lpf
for lpf=a_lpf
 if ~isempty(lpf.raw)
  dd_raw=d_raw(lpf.raw);
  raw1=lpf.raw(1);
 end
 switch lpf.lib
 case 'plwin'
  [dd_data,upar]=plwin(lpf.par,d_parbl,dd_raw);
  dd_data=[];
  d_parbl(42+(1:20))=upar;
 case 'alt_decoder'
  [dd_data]=alt_decoder(lpf.par,d_parbl,dd_raw);
 case 'clutter'
  [dd_data,upar,dd_raw]=clutter(lpf.par,d_parbl,dd_raw);
  d_parbl(42+(1:20))=upar;
  if ~isempty(lpf.raw)
   d_raw(lpf.raw)=dd_raw;
  end
 case 'resampler' % par(1)=[nsamp nrep decimation] boxcar!
  lraw=fix(numel(dd_raw)/lpf.nrep/lpf.par(3))*lpf.par(3);
  dd_raw=reshape(dd_raw,lpf.par(1),lpf.nrep); %reshape into single profiles
  dd_raw=sum(reshape(dd_raw(1:lraw,:),lpf.par(3),[]),1);
  d_raw(raw1+(1:numel(dd_raw)))=dd_raw(:);
  dd_data=[];
 case 'fir' % par=[nsamp nrep fir]
  dd_raw=conv2(reshape(dd_raw,lpf.par(1),[]),flipud(lpf.par(3:end)),'valid'); %only valid
  d_raw(raw1+(1:numel(dd_raw)))=dd_raw(:);
  dd_data=[];
 case 'pulse2pulse' % par=[nsamp nrep ngates nlags]
  if numel(dd_raw)<lpf.par(1)*lpf.par(4)*2
   dd_data=zeros(lpf.par(3)*lpf.par(4),1);
  else
   draw=reshape(dd_raw,lpf.par(1),[]);
   acf=ifft(abs(fft(draw(1:lpf.par(3),:),[],2)).^2,[],2);
   dd_data=reshape(acf(:,1:lpf.par(4)),[],1)/size(acf,2);
  end
 end
 fac=(diff(lpf.p)+1)/lpf.nrep;
 d_data(lpf.data+(1:numel(dd_data)))=dd_data/fac;
end
endt=d_parbl(7)*(lpf.nrep-lpf.p(2)-1)/lpf.nrep;
d_parbl(1:6)=datevec(datenum(d_parbl(1:6))-endt/86400);
d_parbl(7)=d_parbl(7)*fac;
return
