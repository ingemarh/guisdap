function peak=find_plf_peak(s,h,hlim,p,npoly,zscale,fft)
global fpp_plot
if fft
 %100 kHz, 1km resolution fast but need correction sometimes
 df=.1; lf=round(diff(zscale/df)); freqs=(0:lf-1)/(lf-1)*diff(zscale)+zscale(1);
 df=1/(lf-1)*diff(zscale);
 lf=length(freqs); freq=zeros(length(freqs),s);
 hf=min(hlim(:,1)):max(hlim(:,2)); lh=length(hf);
 fmat=zeros(lf,lh); fpow=(hlim(1)./hf).^2;	%take r^2 effect into account
 hf=log(hf); peak=ones(s,2)*NaN;
else
 peak=ones(s,1)*NaN;
end
loghl=log(hlim);
if fpp_plot
 oldfig=gcf; figure(9)
end
for i=1:s
 if size(hlim,1)>=s
  d=find(h(:,i)>hlim(i,1) & h(:,i)<hlim(i,2) & isfinite(p(:,i)));
 else
  d=find(h(:,i)>hlim(1) & h(:,i)<hlim(2) & isfinite(p(:,i)));
 end
 npol=min(npoly,round(length(d)/2)-1);
 if npol>1
  logh=log(h(d,i));		%log(h) to reduce noisy high alt
  [poly,S,mu]=polyfit(logh,p(d,i),npol);
  dpoly=polyder(poly); j=roots(dpoly); hl=(loghl-mu(1))/mu(2);
  ddpoly=polyder(dpoly);
  dd=find(~imag(j) & j>hl(1) & j<hl(2) & polyval(ddpoly,j)<0);
  if ~isempty(dd)
   peak(i)=max(polyval(poly,j(dd)));	%find cutoff
  end
  if any(fpp_plot==i) & ~fft
   plot(p(d,i),h(d,i),polyval(poly,logh,[],mu),h(d,i)),hold on
  end
  if fft
   f=round((polyval(poly,hf,[],mu)-zscale(1))/diff(zscale)*(lf-1));
   d=find(f>-1 & f<lf & hf>logh(1) & hf<logh(end)); f=f+(0:lh-1)*lf+1;
   fmat(f(d))=fpow(d);		%"plot" into matrix
   freq(:,i)=sum(fmat,2);	%integrate along height
   fmat(f(d))=0;		%reset "plot window"
   [d,j]=max(freq(:,i)); peak(i,2)=freqs(j);	%find strongest line
   if any(fpp_plot==i)
    hold on,plot(freqs,freq(:,i))
   end
  end
 end
end
if fpp_plot
 hold off
 figure(oldfig)
end

if fft
 peak={peak freqs freq};
end
