function OK=satch(el,secs,inttime)
global lpg_lag lpg_wr lpg_nt lpg_ri lpg_ra lpg_ND lpg_bcs lpg_dt lpg_bac vc_penvo
global d_data a_satch
global callev calstd

if isempty(lpg_lag)
  OK=1; return
end
if ~isfield(a_satch,'filled')
  if ~isfield(a_satch,'sigma'), a_satch.sigma=4; end	% detection limit sig
  if ~isfield(a_satch,'sigmab'), a_satch.sigmab=3; end	% detection limit cal
  if ~isfield(a_satch,'skip'), a_satch.skip=0; end	% no points to skip
  if ~isfield(a_satch,'clutter'), a_satch.clutter=0; end% no clutter red points
  if ~isfield(a_satch,'repair'), a_satch.repair=0; end	% for alt codes
  if ~isfield(a_satch,'prep'), a_satch.prep=max(vc_penvo); end% p_rep
  if ~isfield(a_satch,'plot'), a_satch.plot=0; end	% plot window
  if ~isfield(a_satch,'lpg_skip'), a_satch.lpg_skip=[0 0]; end% lpg to skip
  a_satch.opts=optimset(optimset('fminsearch'),'display','off');
  a_satch.filled=1;
  disp('Warning: Satellite check works only for single RC prog exps,')
  disp('         starting with the 2nd integration period')
end

C=tan(el*pi/180);
Nsat=0; j=0; x0max=0;
n_echo=6; min_v=.065; skip=a_satch.skip;

lp=setdiff(find(lpg_lag==0 & (lpg_bcs=='s' | lpg_bcs=='x')),a_satch.lpg_skip);
nclutter(1:length(lp))=a_satch.clutter;
nsp_no_use(1:length(lp))=a_satch.repair;
sigma(1:length(lp))=a_satch.sigma;
lpgused=lp; ii=0;
for i=lp
 j=j+1; ii=ii+1;
 addr=(skip:lpg_nt(i)-1)*lpg_ri(i)+lpg_ra(i)+1;
 N=lpg_ND(i)*inttime/(a_satch.prep*1e-6);
 dat=real(d_data(addr));
 [pb,th,L,x0]=sat_check(sigma(ii),n_echo,min_v,full(lpg_wr(:,i))/lpg_ND(i),lpg_dt(i),dat,N,C,nclutter(ii));
 pb=pb(find(pb<=length(dat)-L+1+nsp_no_use(ii)));
 if a_satch.plot
  eval(['dat' num2str(j) '=[dat th]; pc' num2str(j) '=pb+round(L/2);'])
 end
 if ~isempty(th)
  d_data(addr)=th;
  x0max=max([x0max;x0]);
 end
 Nsat=Nsat+length(pb);
end
Nsatb=0;
lp=setdiff(find(lpg_lag==0 & (lpg_bcs=='b' | lpg_bcs=='c')),a_satch.lpg_skip);
sigma(1:length(lp))=a_satch.sigmab;
lpgused=[lpgused lp]; ii=0;
for i=lp
 j=j+1; ii=ii+1;
 addr=(skip:lpg_nt(i)-1)*lpg_ri(i)+lpg_ra(i)+1;
 dat=real(d_data(addr)); dat_m=median(dat);
 data=dat;
 d=find(data>dat_m+sigma(ii)*std(data));
 d1=d;
 while ~isempty(d)
  data(d)=dat_m;
  Nsatb=Nsatb+length(d);
  d=find(data>dat_m+sigma(ii)*std(data));
  d1=[d1;d];
 end
 if a_satch.plot
  eval(['dat' num2str(j) '=dat; pc' num2str(j) '=d1;'])
 end
 d_data(addr)=data;
end
lp=find(lpg_lag==0 & lpg_bcs==99);
sigma(1:length(lp))=a_satch.sigmab;
ii=0;
if isempty(callev), callev=ones(size(lp))*Inf; calstd=callev; end
for i=lp
 ii=ii+1;
 addr=(skip:lpg_nt(i)-1)*lpg_ri(i)+lpg_ra(i)+1;
 cal=real(d_data(addr))/lpg_ND(i);
 b=lpg_bac(i);
 bac=median(real(d_data((skip:lpg_nt(b)-1)*lpg_ri(b)+lpg_ra(b)+1)))/lpg_ND(b);
 newlev=mean(cal)-bac;
 d=(newlev-callev(ii))/calstd(ii);
 if d>sigma(ii)
   fprintf('Sat filling cal, %.1f, replacing...\n',d)
   d_data(addr)=lpg_ND(i)*(bac+callev(ii));
 else
   calstd(ii)=std(cal);
   callev(ii)=newlev;
 end
end
if Nsat>0 %| Nsatb>0
 if a_satch.plot
  figure(a_satch.plot), set(gcf,'Name','Satellites detected')
  for i=1:j
   eval(['dat=dat' num2str(i) '; pc=pc' num2str(i) ';'])
   subplot(j,1,i)
   plot(dat); hold on
   plot(pc,zeros(size(pc)),'o'), hold off
   ylabel(sprintf('%s_{%d}',lpg_bcs(lpgused(i)),lpgused(i)));
  end
  drawnow
 end
 fprintf('Warning: Satellite detection (%d %.1f) -- skipping dump\n',Nsat,x0max)
end
OK=Nsat==0;
return

function [pb,dat_m,L,x0]=sat_check(sigma,n_echo,min_v,wr,dt,dat,N,C,nbigsig)
global p_dtau a_satch

dat_m=[]; x0=[];

%calculate diff of profile normalised with std
S_c=dat/sqrt(N);
S_c(1:nbigsig)=1e2*S_c(1:nbigsig);
mS_c=mean([S_c(1:(end-1)) S_c(2:end)],2);
dat_s=diff(dat)./mS_c;

%calculate pulse shape and diff in sampling intervals
%if el low an echoe would be wider (5km wide beam)
Le=floor(5/C/.15/p_dtau);
if Le>1
 wr=conv(wr,ones(Le,1)/Le);
end
d=find(wr>min_v); d1=d(1)-dt; d2=d(end)+dt; d=rem(d2-d1+1,dt);
if d
 if d<dt/2
  d1=d1+fix(d/2); d2=d2-fix((d+1)/2);
 else
  d=dt-d;
  d1=d1-fix((d+1)/2); d2=d2+fix(d/2);
 end
end
while d2>length(wr)
 d1=d1+fix(dt/2); d2=d2-fix((dt+1)/2);
end
wr_dt=mean(reshape(wr(d1:d2),dt,[]))';
L=length(wr_dt);
wr_diff=diff(wr_dt);

%convolve diff of profile with diff of pulse, find possible echoes
con=conv(dat_s,flipud(wr_diff)/sqrt(sum(abs(wr_diff))));
[y,pb]=sort(con);
pb=flipud(pb); y=flipud(y);
d=find(y>sigma);
pb=pb(d); y=y(d);
if isempty(d)
  return
end

%remove neighbouring echoes
i=1;
while length(pb)>i
  f=find(abs(pb(i)-pb(i:end))<=2);
  s=find(abs(pb(i)-pb(i:end))>2);
  pbi=mean(pb(f+i-1));
  y(i)=mean(y(f+i-1));
  [a,b]=min(abs(pb(f+i-1)-pbi));
  pb(i)=round(pbi);
  pb=[pb(1:i);pb(s+i-1)];
  y=[y(1:i);y(s+i-1)];
  i=i+1;
end
if length(pb)>n_echo
  pb=pb(1:n_echo);
  y=y(1:n_echo);
end

%construct matrix of theor profiles, one col for each echo
th=zeros(2*L+length(dat),length(pb));
h1=pb+1;
h2=h1+L-1;
for i=1:length(pb)
  th(h1(i):h2(i),i)=wr_dt;
end
th=th(1+L:end-L,:); pb=pb-L;

x0=y; x=x0; n_sat=0; l=ceil(L/2);

fit_sat_echo=inline('norm(P1-P2*x)',2);
while length(x)>n_sat & ~isempty(x0)
%make a profile with echoes replaced by polynoms
  dat_m=dat;
  for i=1:length(pb)
    p=pb(i)+[(1-l:1) L+(0:l)].';
    p=p(find(p>0 & p<=length(dat)));
    deg=fix(2*(length(p)-1)/(l+1)); mp=mean(p);
    P=polyfit(p-mp,log(dat_m(p)),deg);
    p=pb(i)+(1:L); p=p(find(p>0 & p<=length(dat)));
    dat_m(p)=exp(polyval(P,p-mp));
  end
  a=find(sum(th,2));
  dat_c=dat(a)-dat_m(a);

  [x,err]=fminsearch(fit_sat_echo,x0,a_satch.opts,dat_c./S_c(a),th(a,:));
  sat=find(x>sigma);
  x0=x(sat);
  th=th(:,sat);
  pb=pb(sat);
  [xx,i]=sort(x0); i=flipud(i); x0=x0(i); th=th(:,i); pb=pb(i);
  n_sat=length(sat);
end

%if n_sat>0
%y',x0'
% th=th*x0;
% figure(9),subplot(3,1,1)
% plot([dat dat_m]);
% subplot(3,1,2)
% plot([th dat_c])
% subplot(3,1,3)
% plot(1:length(dat_s),dat_s,(1:length(con))-round(L/2),con);drawnow
%end
