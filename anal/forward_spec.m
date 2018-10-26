function forward_spec(data,var,lpgs,f_womega)
global lpg_lag p_dtau r_spec r_freq r_lag r_acf r_ace di_spectra
weight=sum(abs(f_womega),2);
nd=length(weight);
weight(nd/2+1:end)=weight(1:nd/2);
wt=var(1:nd)./weight;
lags=round(lpg_lag(lpgs));
lag=sort(unique(lags)); acf=zeros(size(lag)); err=acf;
for l=1:length(lag)
 ad=find(lags==lag(l));
 adi=ad+nd/2;
 acf(l)=sum(data(ad)./weight(ad)./wt(ad))/sum(1../wt(ad));
 err(l)=sum(1../weight(ad)./wt(ad))/sum(1../wt(ad));
 if wt(adi)~=0
  acf(l)=acf(l)+j*sum(data(adi)./weight(adi)./wt(adi))/sum(1../wt(adi));
 end
end
%[err(1:5)/mean(err) std(err)/mean(err)]
%keyboard
if di_spectra<0
 if isempty(r_acf)
  r_acf=acf';
  r_ace=err';
  r_lag=lag'*p_dtau*1e-6;
 else
  lrf=size(r_lag,1); lf=length(lag);
  if lrf<lf
   r_lag(end+1:lf,:)=NaN;
   r_acf(end+1:lf,:)=NaN;
   r_ace(end+1:lf,:)=NaN;
  elseif lrf>lf
   lag(end+1:lrf)=NaN;
   acf(end+1:lrf)=NaN;
   err(end+1:lrf)=NaN;
  end
  r_lag(:,end+1)=lag'*p_dtau*1e-6; r_acf(:,end+1)=acf'; r_ace(:,end+1)=err';
 end
else
 dt=median(diff(lag));
 l=find(err>mean(err)+3*di_spectra*std(err)); acf(l)=[]; lag(l)=[];
 ml=max(lag); nl=round(ml/dt); l=(0:nl)/nl*ml; fl=fliplr(find(lag));
 aend=0; if imag(acf(end))==0, aend=1; end
 acf=interp1([-lag(fl) lag],[conj(acf(fl)) acf],l,'spline');
 if aend, acf(end)=real(acf(end)); end
 [s,f]=acf2spec(acf.',l*p_dtau*1e-6);
 if isempty(r_freq)
  r_freq=f'; r_spec=s;
 else
  lrf=size(r_freq,1); lf=length(f);
  if lrf<lf
   r_freq(end+1:lf,:)=NaN;
   r_spec(end+1:lf,:)=NaN;
  elseif lrf>lf
   f(end+1:lrf)=NaN;
   s(end+1:lrf)=NaN;
  end
  r_freq(:,end+1)=f'; r_spec(:,end+1)=s;
 end
%plot(f,s),drawnow
end


function [f,w]=acf2spec(acf,lag,m)
[k,l]=size(acf);
if nargin<3
 m=1:l;
end
% add window
for mm=m
 kkk=find(acf(:,mm)==0);
 if isempty(kkk), kk=k; else, kk=kkk(1); end
 wind=hamming(2*kk); acf(1:kk,mm)=acf(1:kk,mm).*wind(kk+1:end);
end
acf(1,m)=real(acf(1,m));
if all(imag(acf(k,m))==0)
 sp=real(fft([acf(1:k-1,m);conj(acf(k:-1:2,m))]));
 k=k-1;
else
 sp=real(fft([acf(1:k,m);zeros(1,length(m));conj(acf(k:-1:2,m))]));
end
f=[sp(k+1:2*k,:);sp(1:k,:)];
if nargin>1
 df=1/mean(diff(lag)); w=(-k:k-1)*df/(2*k); f=f/df;
end

function w=hamming(n)
w=.54-.46*cos(2*pi*(0:n-1)'/(n-1));
