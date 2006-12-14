function [OK,ad_sat]=satch(el,secs,inttime)
global lpg_lag lpg_wr lpg_w lpg_nt lpg_ri lpg_ra lpg_ND lpg_bcs lpg_dt lpg_bac vc_penvo lpg_code lpg_h
global d_data a_satch a_code
global ad_range ad_w ad_code ad_lag ad_bac
global calold

ad_sat=[];
if isempty(lpg_lag)
  OK=1; return
end
if ~isfield(a_satch,'filled')
  if ~isfield(a_satch,'sigma'), a_satch.sigma=4; end	% detection limit sig
  if ~isfield(a_satch,'sigmab'), a_satch.sigmab=3; end	% det lim bac+cal
  if ~isfield(a_satch,'sigmac'), a_satch.sigmac=Inf; end% det lim cal fill
  if ~isfield(a_satch,'calsecs'), a_satch.calsecs=99; end% time lim cal fill
  if ~isfield(a_satch,'skip'), a_satch.skip=0; end	% no points to skip
  if ~isfield(a_satch,'clutter'), a_satch.clutter=0; end% no clutter red points
  if ~isfield(a_satch,'clutfac'), a_satch.clutfac=1000; end% amount of clutter
  if ~isfield(a_satch,'repair'), a_satch.repair=0; end	% for alt codes
  if ~isfield(a_satch,'prep'), a_satch.prep=max(vc_penvo); end% p_rep
  if ~isfield(a_satch,'plot'), a_satch.plot=0; end	% plot window
  if ~isfield(a_satch,'lpg_skip'), a_satch.lpg_skip=[]; end% lpg to skip
  if ~isfield(a_satch,'cut'), a_satch.cut=0; end	% cut or trash
  if ~isfield(a_satch,'do'), a_satch.do=1; end		% do satch
  a_satch.opts=optimset(optimset('fminsearch'),'display','off');
  a_satch.filled=1;
  if a_satch.do
   disp('Warning: Satellite check works only for single RC prog exps,')
   disp('         starting with the 2nd integration period')
  end
end
if ~a_satch.do
 OK=1; return
end

C=tan(el*pi/180);
Nsat=0; j=0; x0max=0; pbmax=NaN;
n_echo=6; min_v=.065; skip=a_satch.skip;

%check signal
lp=setdiff(find(lpg_lag==0 & (lpg_bcs=='s' | lpg_bcs=='x') & lpg_bac~=0),a_satch.lpg_skip);
if ~isempty(a_code)
 lp=lp(find(ismember(lpg_code(lp),unique(a_code))));
end
nclutter(1:length(lp))=a_satch.clutter;
nsp_no_use(1:length(lp))=a_satch.repair;
sigma(1:length(lp))=a_satch.sigma;
lpgused=lp; ii=0; sat_ran=[];
for i=lp
 j=j+1; ii=ii+1;
 addr=(skip:lpg_nt(i)-1)*lpg_ri(i)+lpg_ra(i)+1;
 N=lpg_ND(i)*inttime/(a_satch.prep*1e-6);
 dat=real(d_data(addr));
 [pb,th,L,x0]=sat_check(sigma(ii),n_echo,min_v,full(lpg_wr(:,i))/lpg_ND(i),lpg_dt(i),dat,N,C,[nclutter(ii) a_satch.clutfac]);
 ind=find(pb<=length(dat)-L+1+nsp_no_use(ii));
 pb=pb(ind)+L/2; x0=x0(ind);
 if ind>0
   sat_ran=[sat_ran;[lpg_h(i)+(pb-1)*lpg_dt(i)...
           ones(size(pb))*[2*lpg_w(i)-lpg_dt(i) L*lpg_dt(i) lpg_code(i)]]];
 end
 if a_satch.plot
  eval(['dat' num2str(j) '=[dat th]; pc' num2str(j) '=pb;'])
 end
 if ~isempty(th)
  d_data(addr)=th;
  [x0max,ind]=max([x0max;x0]); pbmax=[pbmax;pb]; pbmax=pbmax(ind);
 end
 Nsat=Nsat+length(pb);
end

%check back and cal
Nsatb=0;
lp=setdiff(find(lpg_lag==0 & (lpg_bcs=='b' | lpg_bcs=='c')),a_satch.lpg_skip);
if ~isempty(a_code)
 lp=lp(find(ismember(lpg_code(lp),unique(a_code))));
end
sigma(1:length(lp))=a_satch.sigmab;
lpgused=[lpgused lp]; ii=0;
for i=lp
 ii=ii+1;
 addr=(skip:lpg_nt(i)-1)*lpg_ri(i)+lpg_ra(i)+1;
 if length(addr)>1
  j=j+1;
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
end

%check cal filled (Unstable, uses prevoius dump)
if isfinite(a_satch.sigmac)
 lp=setdiff(find(lpg_lag==0 & lpg_bcs=='c'),a_satch.lpg_skip);
 if ~isempty(a_code)
  lp=lp(find(ismember(lpg_code(lp),unique(a_code))));
 end
 sigma(1:length(lp))=a_satch.sigmac;
 ii=0;
 if isempty(calold) | calold.secs-secs>a_satch.calsecs
  calold.lev=ones(size(lp))*Inf; calold.std=calold.lev;
 end
 calold.secs=secs;
 for i=lp
  ii=ii+1;
  addr=(skip:lpg_nt(i)-1)*lpg_ri(i)+lpg_ra(i)+1;
  if ~isempty(addr)
   cal=real(d_data(addr))/lpg_ND(i);
   b=lpg_bac(i);
   bac=median(real(d_data((skip:lpg_nt(b)-1)*lpg_ri(b)+lpg_ra(b)+1)))/lpg_ND(b);
   newlev=(mean(cal)-bac)/inttime;
   d=(newlev-calold.lev(ii))/calold.std(ii);
   if d>sigma(ii)
    fprintf('Sat filling cal, %.1f, replacing...\n',d)
    d_data(addr)=lpg_ND(i)*(bac+inttime*calold.lev(ii));
   else
    calold.std(ii)=std(cal);
    calold.lev(ii)=newlev;
   end
  end
 end
end

%report the result
OK=Nsat==0;
if ~OK %| Nsatb>0
 if a_satch.plot
  drawnow, figure(a_satch.plot), set(gcf,'Name','Satellites detected')
  for i=1:j
   eval(['dat=dat' num2str(i) '; pc=pc' num2str(i) ';'])
   subplot(j,1,i)
   plot(dat); hold on
   plot(pc,zeros(size(pc)),'o'), hold off
   ylabel(sprintf('%c_{%d}',lpg_bcs(lpgused(i)),lpgused(i)));
  end
  drawnow
 end
 fprintf('Warning: Satellite detection (%d %.1f %.0f) -- skipping ',Nsat,x0max,pbmax)
 if a_satch.cut
  for j=1:size(sat_ran,1)
   arang=2*abs(ad_range-sat_ran(j,1));
   d=find(((ad_bac==0 & arang<sat_ran(j,2)) | ...
 (ad_bac~=0 & arang<=ad_lag+sat_ran(j,3))) & ad_range>0 & ad_code==sat_ran(j,4));
   ad_sat=union(ad_sat,d);
  end
  fprintf('%d ranges\n',length(ad_sat))
  OK=1;
 else
  fprintf('dump\n')
 end
end
return

function [pb,dat_m,L,x0]=sat_check(sigma,n_echo,min_v,wr,dt,dat,N,C,nbigsig)
global p_dtau a_satch

dat_m=[]; x0=[];

%calculate diff of profile normalised with std
S_c=dat/sqrt(N);
S_c(1:nbigsig)=nbigsig(2)*S_c(1:nbigsig(1));
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
