function ratio=calib_pl_ne(pl_dir,expt,gate,plots)
% function ratio=calib_pl_ne(pl_dir,expt,gate,plots)
% GUISDAP 8.4  8 Oct 2004  Copyright EISCAT
% Calibration routine of radar constant using simultaneous wide band plasma
% lines and GUISDAP results
% Using setup in pl_def.m in experiment directory
% input: pl_dir Directory containing integrated plasma lines (needed)
%        expt   Name of experiment (or guessed from pl_dir)
%        gate   Gate number to fit
%        plots  Display a number of plots for internal checks
% output: ratio Vector containing
%                Density ratio
%                Error bar of dens ratio
%                Number of "good" points used
%                Calculated new Magic constant
%                Emperical best guess taking Te changes into account
% see also CALIB_NE, PLASMA_SUMMARY
if nargin<2, expt=[]; end
gates=[];
if nargin<3
 gate=[];
else
 gates=0;
end
if nargin<4, plots=0; end
%if isempty(plots), plots=0; end
global Time par1D par2D axs r_Magic_const DATA_PATH local START_TIME END_TIME fpp_plot pl vizufig

edge_dist=10; overlap=2;

%Get plasmaline data
disp('Reading plasmaline data')
pgcf=findobj('type','figure','userdata',7);
if isempty(pgcf), pgcf=gupfigure; end
ogcf=gcf;
[pl,p]=plasma_summary(pl_dir,[],expt,gates,plots);
set(0,'currentfigure',ogcf)
if isempty(gate) | gate==0, gate=p.gate; end
if isempty(gate), gate=1; end

re=6370; ratio=[];
%First guess of altitudes (Not very important)
alt=re*sqrt(1+p.ran(gate,:)/re.*(p.ran(gate,:)/re+2*sin(p.ele/57.2957795)))-re;
if p.ele==0, alt=[150 450]; end
%Read in the analysed data
lf=vizu('verbose',alt,'L1 AE');
for i=1:2
 hlim(:,i)=p.ran(gate,i)*(p.ran(gate,i)/re+2*sin(par1D(:,2)/57.2957795));
 hlim(:,i)=p.ran(gate,i)*(p.ran(gate,i)/re+2*sin(par1D(:,2)/57.2957795));
end
hlim=re*sqrt(1+hlim/re)-re;
fgup=fullfile(DATA_PATH,'.gup');
Magic_const=1;
if exist('r_code','var'), a_code=r_code; end
if ~isempty(r_Magic_const)
 Magic_const=mean(r_Magic_const);
elseif exist(fgup)
 load(fgup,'-mat')
 for i=1:size(extra,1),eval(extra(i,:));end
end
if length(r_Magic_const)>1 & exist('a_code','var'), Magic_const=mean(r_Magic_const(a_code)); end

d=find(pl.t(2,:)>max([Time(1) datenum(START_TIME)]) & ...
  pl.t(1,:)<=min([Time(end) datenum(END_TIME)]));
nfl=length(d);
if nfl==0
 error('You stupid! Choose a better data set.')
else
 pl.t=pl.t(:,d); pl.f=pl.f(:,:,d); pl.s=pl.s(gate,:,:,d);
end
nfreq=size(pl.s,3); nfft=size(pl.s,2); pl.s=reshape(pl.s,nfft,nfreq,nfl);

%make pl_matrix
fres=.1; %100kHz
freq_meas=[min(floor(abs(pl.f(:)/fres/1e6))) max(ceil(abs(pl.f(:)/fres/1e6)))]*fres;
nplf=round(diff(freq_meas)/fres)+1;
plm=zeros(nplf,nfl);
pln=zeros(nplf,nfl);
sf=(0:nplf-1)*(diff(freq_meas)+fres)/nplf+freq_meas(1);
f=round((1e-6*abs(pl.f)-freq_meas(1))/fres)+1;
for j=1:nfreq
 for i=1:nfft
  for k=1:nfl
   pln(f(i,j,k),k)=pln(f(i,j,k),k)+1;
   plm(f(i,j,k),k)=plm(f(i,j,k),k)+pl.s(i,j,k);
  end
 end
end
d=find(pln>0); plm(d)=plm(d)./pln(d);
d=find(pln==0); plm(d)=NaN;
d=find(plm<0); plm(d)=0;

%find pl peaks
plpeak=ones(2,nfl,nfreq)*NaN;
validpl=pl.f(find(isfinite(pl.f)));
s=std(validpl(find(abs(validpl-median(validpl))<std(validpl))));
for j=1:nfreq
 for i=1:nfl
  [a,b]=max(pl.s(:,j,i));
  if a>s && b>edge_dist && b<nfft-edge_dist+1
   plpeak(:,i,j)=[pl.f(b,j,i);a];
  end
 end
end
if isempty(find(isfinite(plpeak)))
 error('No plasma lines found')
end
plpeak_c=ones(nfl,2)*NaN;
for i=p.updown
 [a,b]=max(plpeak(2,:,(1:p.nup_d)+i*p.nup_d),[],3);
 for j=1:nfl
  plpeak_c(j,i+1)=plpeak(1,j,b(j)+i*p.nup_d);
 end
end
plf=ones(nfl,1)*NaN;
if length(p.updown)==2
 d=find(abs(sum(plpeak_c,2))<5*abs(p.df));
 if isempty(d)
  error('No simultanous up- and downshifted plasma lines found')
 end
 plf(d)=mean(abs(plpeak_c(d,:)),2)/1e6;
else
 plf=abs(plpeak_c(:,1+p.updown))/1e6;
end
if plots>1
 ogcf=gcf; set(0,'currentfigure',pgcf)
 plot([abs(plpeak_c)/1e6 plf]), pause
 set(0,'currentfigure',ogcf)
end
% We have (lf Time) vs (plf p_time), now do the comparison and
% find the density ratio (need a few iterations)
disp('Fitting')
d=find(isfinite(plf));
Gtime=mean(Time); Ptime=mean(pl.t); maxs=overlap/2*mean(diff(Time));
ip=[]; il=[];
for i=d'
 [s j]=min(abs(Ptime(i)-Gtime));
 if s<maxs && isfinite(lf(j))
  ip=[ip i]; il=[il j];
 end
end
if isempty(ip), error('No overlapping times GUISDAP vs PL'), end
s=size(Time,2); h=par2D(:,:,2); r0=1; mr=1; mrpp=100;
freq_th=[0 10];
nloop=0;
while (mr==r0 | abs(mr-1)>.01) & nloop < 16
 nloop=nloop+1
 mrp=abs(mr-1);
 %if mrp>mrpp
 % error('Cannot fit this')
 %end
 if mr~=r0, mrpp=mrp; end
 mr=mr*r0; r0=mr;
 lf=8.98e-6*sqrt(par2D(:,:,3))/mr.*sqrt(1+3*7.52e5*(p.fradar/3e8)^2*par2D(:,:,4)./par2D(:,:,3)*mr^2);
 if plots>1
  ogcf=gcf; set(0,'currentfigure',pgcf)
  fpp_plot=1:100:s;
 end
 lf=find_plf_peak(s,h,hlim,lf,16,freq_th,1);
 if plots>1
  fpp_plot=[]; pause
  set(0,'currentfigure',ogcf)
 end
 peak_lf=lf{1}(:,2);
 r=peak_lf(il)./plf(ip); r2=r.*2;
 mr2=median(r2); sr2=std(r2);
 good=find(abs(r2-mr2)<=p.maxe*sr2);
 bad=find(abs(r2-mr2)>p.maxe*sr2);
 mr=median(r(good)); sr2=std(r2(good));
 if r0==1
  axs(3)=copyobj(axs(1),vizufig);
  hold(axs(3),'on')
  plot(axs(3),mean(pl.t)+eps,plf,'+g',mean(Time),peak_lf,'+b')
  hold(axs(3),'off')
  set(axs(3),'color','none')
  set(vizufig,'colormap',1-gray)
  plm=plm/max(max(plm))*length(get(vizufig,'colormap'));
  delete(get(axs(1),'children')), delete(get(axs(1),'ylabel'))
  for i=1:nfl
   surface(axs(1),pl.t(:,i),[sf 2*sf(end)-sf(end-1)]'-fres/2,[plm(:,i);plm(end,i)]*ones(1,2))
  end
  set(axs(1),'xtick',[],'ytick',[],'ticklength',[0 0])
  if local.matlabversion>=7, linkaxes(axs([1 3])), end
  pmax=ceil(max([plf;peak_lf(il(good))]));
  rx=[0 pmax];
  plot(axs(2),plf(ip(good)),peak_lf(il(good)),'o',plf(ip(bad)),peak_lf(il(bad)),'o',rx,rx*mr,'-',rx,rx,'-')
  hy=ylabel(axs(2),'Calculated plasmaline peak (MHz)');
  hx=xlabel(axs(2),'Measured plasmaline peak (MHz)');
  set(hx,'color','green'), set(hy,'color','blue')
  pos=get(axs(2),'position'); pos(3)=.5;
  set(axs(2),'xlim',[0 pmax],'ylim',[0 pmax],'position',pos), axis(axs(2),'square')
  freq_th=freq_meas;
 end
end
mr2=(mr*r0)^2; sr2=sr2*mr2;
newMagic=Magic_const/mr2;
%Need some overshoot for Te changes
better_guess=newMagic*exp(-log(mr2)/15); % 15 OK for 22May04 ESR
delete(findobj(vizufig,'UserData','Results'))
text(axs(2),pmax*1.04,pmax/2,sprintf('Density ratio=%.2f\\pm%.2f\nMagic const used=%g\n\nheight=%.0f-%.0f km\ngreen circles > %g\\sigma\n\nsuggested Magic const=%.2f',mr2,sr2,Magic_const,mean(hlim),p.maxe,better_guess),'horiz','left','UserData','Results')
if abs(mr2-1)>.01
 fprintf('Try Magic_const=%.2f; (%.2f)\n',better_guess,newMagic)
end
%text(3,.2,sprintf('Try Magic const=%.2f (%.2f)',newMagic,better_guess))
ratio=[mr2 sr2 length(good) newMagic better_guess];
