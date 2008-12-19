function forward_spec(data,var,lpgs,f_womega)
global lpg_dt lpg_lag p_dtau r_spec r_freq

dt=median(lpg_dt(lpgs));
weight=sum(f_womega,2);
nd=length(weight);
weight=weight.*var(1:nd);
lags=lpg_lag(lpgs);
lag=sort(unique(lags)); acf=zeros(size(lag)); err=acf;
for l=1:length(lag)
 ad=find(lpg_lag(lpgs)==lag(l));
 adi=ad+nd/2;
 acf(l)=sum(data(ad)./weight(ad))/sum(1../weight(ad));
 %err(l)=1/sum(1../weight(ad));
 if weight(adi)~=0
  acf(l)=acf(l)+j*sum(data(adi)./weight(adi))/sum(1../weight(adi));
  %err(l)=1/(sum(1../weight(ad))+sum(1../weight(adi)));
 end
end
ml=max(lag); nl=round(ml/dt); l=(0:nl)/nl*ml; fl=find(lag);
acf=interp1([-lag(fl(1)) lag],[conj(acf(fl(1))) acf],l,'spline');
[s,f]=acf2spec(acf.',l*p_dtau*1e-6);
if isempty(r_freq)
 r_freq=f'; r_spec=s;
else
 lrf=size(r_freq,2); lf=length(f);
 if lrf<lf
  r_freq(end+1:lf,:)=NaN;
  r_spec(end+1:lf,:)=NaN;
 elseif lrf>lf
  f(end+1:lrf)=NaN;
  s(end+1:lrf)=NaN;
 end
 r_freq(:,end+1,:)=f'; r_spec(:,end+1)=s;
end
%plot(f,s),drawnow
