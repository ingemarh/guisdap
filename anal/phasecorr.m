function [phd,vel,phdd]=phasecorr(draw,penv,dt,nbit,ind,lo)
%function phd=phasecorr(draw,penv,dt,nbit,ind,lo)
%Calculate the Dopplershift caused by phasepushing in klystron
global ch_fradar p_dtau
%penv=vcg_penv(:,1:64); draw=d_raw; dt=10; nbit=64-1;
%dt=15; nbit=60-2; draw=d_raw(1:end/2);
if nargin<6, lo=[]; end
if nargin<5, ind=[]; end

ncod=size(penv,2);
dr=reshape(draw,[],ncod);
txsam=size(dr,1);
rdt=dt*p_dtau*1e-6;
phd=[]; phdd=[];

ac=round(penv((1+(1:nbit))*dt,:));
if ~find(ac)
 error('Need to offset vcg_penv')
end
natd=txsam-nbit+1;
atd=zeros(nbit,natd);
for i=1:natd
 for j=1:nbit
  atd(:,i)=atd(:,i)+ac(:,j).*dr(i:nbit-1+i,j);
 end
end
if isempty(ind), [m,ind]=max(sum(abs(atd))); end
ca=angle(atd(:,ind));
t=rdt*((0:(nbit-1))'-(nbit-1)/2);

p=polyfit(t,ca,1);
if 0
 phd=p(1)/2/pi;
end

%aatd=mean(ca);
mphd=2*pi/rdt;
fit_tx=inline('norm(exp(complex(0,P1*x(1)+x(2)))-P2)',2);
[x,f,flag]=fminsearch(fit_tx,p,[],t,atd(:,ind)./abs(atd(:,ind)));
%figure(9), plot(t,[ca angle(exp(complex(0,t*x(1)+x(2))))]), drawnow
if flag
 phd=rem(x(1),mphd)/2/pi;
end

if 0
 atf=angle(ac.*dr(ind:nbit-1+ind,:));
 ph=zeros(1,ncod);
 for i=1:ncod
  p=polyfit(t,atf(:,i),1);
  ph(i)=p(1);
 end
 phd=mean(ph)/2/pi;
end

if ~isempty(lo)
 atf=ac.*dr(ind:nbit-1+ind,:);
 ph=zeros(1,ncod);
 for i=1:ncod
  [x,f,flag]=fminsearch(fit_tx,x,[],t,atf(:,i)./abs(atf(:,i)));
  if flag, ph(i)=x(1); end
 end
 ph=ph(find(ph));
 if ~isempty(ph)
  phd=median(rem(ph,mphd))/2/pi;
  phdd=std(rem(ph,mphd))/2/pi;
 end
end

vel=-phd*3e8/ch_fradar(1)/2;
