function ratio=calib_pl_ne(pl_dir,expt,gate,plots)
% function ratio=calib_pl_ne(pl_dir,expt,plots)
% GUISDAP 8.4  8 Oct 2004  Copyright EISCAT
% Calibration routine of radar constant using simultaneous wide band plasma
% lines and GUISDAP results
% input: pl_dir Directory containing integrated plasma lines (needed)
%        expt   Name of experiment (or guessed from pl_dir)
%        plots  Display a number of plots for internal checks
% output: ratio Vector containing
%                Density ratio
%                Error bar of dens ratio
%                Calculated new Magic constant
%                Emperical best guess taking Te changes into account
%                Number of "good" points used
% see also CALIB_NE
if nargin<2, expt=[]; end
if nargin<3, gate=[]; end
if nargin<4, plots=[]; end
if isempty(expt), expt=pl_dir; end
if strfind(expt,'steffe2')
 nfft=0; nint=1; ngates=3; nlag=175;
 freq=[-3.8 -5.4 3.8 5.4]*1e6; dt=0.6e-6; invert=-1;
 ran=[47 314;189 455;330 597]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=2; skip_if=0;
 if [strfind(expt,'cut') regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_steffe2_\d+@32p')]
  startad=(0:3)*3*nlag+1;
 else
  startad=(0:3)*19619+10*nlag+9*1536+1;
 end
 if isempty(gate), gate=2; end
 %freq=freq([1 3]); startad=startad([1 3]); nup_d=1;
elseif strfind(expt,'ipy')
 nfft=0; nint=1; ngates=3; nlag=50;
 freq=[-4.0 4.0]*1e6; dt=0.6e-6; invert=-1;
 ran=[41 180;151 290;262 400]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=1; skip_if=1;
 if [strfind(expt,'cut') regexp(pl_dir,'\d\d\d\d-\d\d-\d\d_ipy1_\d+@32p')]
  startad=(0:1)*3*nlag+1;
 else
  startad=(0:1)*19898+22*nlag+21*768+21;
 end
 if isempty(gate), gate=2; end
 %freq=freq(2); updown=0; startad=startad(2);
elseif strfind(expt,'plwin')
 nfft=0; nint=1; ngates=3; nlag=240;
 freq=[-4.8 4.8]*1e6; dt=0.4e-6; invert=-1;
 ran=[69 300;170 400;270 501]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=1;
 startad=[1 48165]+19*240+18*2048; skip_if=1;
 if isempty(gate), gate=2; end
 %freq=freq(1); updown=0; startad=startad(1);
elseif strfind(expt,'steffe')
 nfft=128; nint=2; ngates=1; nlag=0;
 freq=[-4 -5.3 4 5.3]*1e6; dt=0.6e-6; invert=-1;
 ran=[182 423.9]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=2;
 startad=1; gate=1; skip_if=0;
elseif strfind(expt,'tau8')
 nfft=128; nint=2; ngates=4; nlag=0;
 freq=4.1*1e6; dt=0.6e-6; invert=-1;
 ran=[53.3 272.4;193.4 412.5;333.4 552.5;473.3 692.4]; fradar=224e6;
 maxe=2; ele=0; updown=0; nup_d=1;
 startad=108679; skip_if=1;
 if isempty(gate), gate=2; end
else
 error('No such experiment defined (steffe2,steffe,plwin,tau8)')
end
if isempty(gate), gate=1; end

%pl_dir='/home/ingemar/gup/mydata/steffel_int_CP@32p/';
%res_dir='gup/results/2004-05-2*_steffe_60@42m/';
%assume similar integration!
global Time par1D par2D axs r_Magic_const DATA_PATH local
edge_dist=10; wrap=9;
nfreq=length(freq);

re=6370; ratio=[];
%First guess of altitudes (Not very important)
alt=re*sqrt(1+ran(gate,:)/re.*(ran(gate,:)/re+2*sin(ele/57.2957795)))-re;
if ele==0, alt=[150 450]; end
%Read in the analysed data
lf=vizu('verbose',alt,'L1 AE');
for i=1:2
 hlim(:,i)=ran(gate,i)*(ran(gate,i)/re+2*sin(par1D(:,2)/57.2957795));
end
hlim=re*sqrt(1+hlim/re)-re;
fgup=fullfile(DATA_PATH,'.gup');
Magic_const=1;
if ~isempty(r_Magic_const)
 Magic_const=mean(r_Magic_const);
elseif exist(fgup)
 load(fgup,'-mat')
 for i=1:size(extra,1),eval(extra(i,:));end
end

%Get plasmaline data
disp('Reading plasmaline data')
[fl,msg]=getfilelist(fullfile(pl_dir,filesep));
if msg, error(msg), end
d=cell2mat({fl.file});
fl=fl(find(d>tosecs(datevec(Time(1))) & d<=tosecs(datevec(Time(end)))));
nfl=length(fl);
if nfl==0
 error('You stupid! Choose a better data set.')
end
endad=startad+(nfft+nlag)*ngates*nint*nfreq/length(startad)-1;
add=[]; for i=1:length(startad), add=[add startad(i):endad(i)]; end
if nlag>0, nfft=2*nlag; end
plspec=zeros(nfft,nfreq,nfl);
p_time=zeros(2,nfl);
for i=1:nfl
 filename=fullfile(fl(i).dir,sprintf('%08d%s',fl(i).file,fl(i).ext));
 load(canon(filename,0))
 p_time(:,i)=datenum(d_parbl(1:6)')+[-d_parbl(7)/86400;0];
 if nlag>0
  d=sum(reshape(d_data(add),ngates,nlag,nint,nfreq),3)/d_parbl(22);
  d=reshape(d(gate,:,1,:),nlag,nfreq);
  d=real(fft([d;zeros(1,nfreq);conj(d(nlag:-1:2,:))]));
  d=d([nfft/2+1:nfft 1:nfft/2],:);
 else
  d=sum(reshape(real(d_data(add)),nfft,ngates,nint,nfreq),3)/d_parbl(22);
  d=reshape(d([nfft/2+1:nfft 1:nfft/2],gate,1,:),nfft,nfreq);
 end
 plspec(:,:,i)=d;
end
if plots
 ogcf=gcf; figure(9)
 imagesc(reshape(plspec,nfft*nfreq,[])), pause
 set(0,'currentfigure',ogcf)
end

%Get spectral shape and find interferences
disp('Removing noise')
spec_shape=median(plspec,3);
filt_shape=spec_shape;
fsh=[filt_shape(end-wrap+1:end,:);filt_shape;filt_shape(1:wrap,:)];
d2=diff(filt_shape,2);
fd2=-std(d2);
for i=1:nfreq
 d=find(d2(:,i)<2*fd2(i));
 for j=d'
  filt_shape(j+(1:3),i)=interp1([-3:-1 5:7]',fsh(j+wrap+[-3:-1 5:7],i),(1:3)','spline');
 end
end
if plots
 ogcf=gcf; figure(9)
 plot([filt_shape spec_shape]), pause
 set(0,'currentfigure',ogcf)
end
interf_spec=spec_shape-filt_shape;
if skip_if
 interf_spec(find(interf_spec~=0))=NaN;
%filt_shape=spec_shape;
end

%Find a timevarying background (Use Tsys instead?)
fp=reshape(median(plspec,1),nfreq,[]);
%fr=mean(spec_shape,1)./median(spec_shape,1);
%fp=fp.*(fr'*ones(1,nfl));
nplspec=zeros(size(plspec));
spnorm=median(filt_shape,1);
for i=1:nfreq
 nplspec(:,i,:)=(filt_shape(:,i)*fp(i,:)/spnorm(i));
end
%Add interference lines to background
nplspec=reshape(reshape(nplspec,nfft*nfreq,[])+interf_spec(:)*ones(1,nfl),nfft,nfreq,[]);

%Remove background and normalise signal
plsig=plspec-nplspec;
if plots
 ogcf=gcf; figure(9)
 imagesc(reshape(plsig,nfft*nfreq,[])), pause
 set(0,'currentfigure',ogcf)
end
plsig=reshape(plsig,nfft*nfreq,[])./(filt_shape(:)*ones(1,nfl));
plsig=reshape(plsig,nfft,nfreq,[]);

%Find frequencies of the maximum point
freq_scale=invert*(-nfft/2:nfft/2-1)/dt/nfft;
df=abs(diff(freq_scale(1:2)));
if plots
 ogcf=gcf; figure(9)
 plot(freq_scale'*ones(1,nfreq)+ones(nfft,1)*freq,plsig(:,:,round(nfl/2))), pause
 set(0,'currentfigure',ogcf)
end
freq_meas=10e-6*abs([freq_scale(1)+freq freq_scale(end)+freq]);
freq_meas=[min(floor(freq_meas)) max(ceil(freq_meas))]/10;
%make pl_matrix
fres=.1; %100kHz
nplf=round(diff(freq_meas)/fres)+1;
plm=zeros(nplf,nfl);
pln=zeros(nplf,1);
sf=(0:nplf-1)*(diff(freq_meas)+fres)/nplf+freq_meas(1);
for j=1:nfreq
 f=round((1e-6*abs(freq_scale+freq(j))-freq_meas(1))/fres)+1;
 for i=1:nfft
  pln(f(i))=pln(f(i))+1;
  plm(f(i),:)=plm(f(i),:)+reshape(plsig(i,j,:),1,[]);
 end
end
d=find(pln>0); plm(d,:)=plm(d,:)./(pln(d)*ones(1,nfl));
d=find(pln==0); plm(d,:)=[]; sf(d)=[];
d=find(plm<0); plm(d)=0;

plpeak=ones(2,nfl,nfreq)*NaN;
s=std(plsig(find(isfinite(plsig))));
for j=1:nfreq
 for i=1:nfl
  [a,b]=max(plsig(:,j,i));
  if a>s & b>edge_dist & b<nfft-edge_dist+1
   plpeak(:,i,j)=[freq_scale(b)+freq(j);a];
  end
 end
end
if isempty(find(isfinite(plpeak)))
 error('No plasma lines found')
end
plpeak_c=ones(nfl,2)*NaN;
for i=updown
 [a,b]=max(plpeak(2,:,(1:nup_d)+i*nup_d),[],3);
 for j=1:nfl
  plpeak_c(j,i+1)=plpeak(1,j,b(j)+i*nup_d);
 end
end
plf=ones(nfl,1)*NaN;
if length(updown)==2
 d=find(abs(sum(plpeak_c,2))<5*df);
 if isempty(d)
  error('No simultanous up- and downshifted plasma lines found')
 end
 plf(d)=mean(abs(plpeak_c(d,:)),2)/1e6;
else
 plf=abs(plpeak_c(:,1+updown))/1e6;
end
if plots
 ogcf=gcf; figure(9)
 plot([abs(plpeak_c)/1e6 plf]), pause
 set(0,'currentfigure',ogcf)
end

% We have (lf Time) vs (plf p_time), now do the comparison and
% find the density ratio (need a few iterations)
disp('Fitting')
d=find(isfinite(plf));
Gtime=mean(Time); Ptime=mean(p_time); maxs=mean(diff(Time))/2;
ip=[]; il=[];
for i=d'
 [s j]=min(abs(Ptime(i)-Gtime));
 if s<maxs
  ip=[ip i]; il=[il j];
 end
end
if isempty(ip), error('No overlapping times GUISDAP vs PL'), end
s=size(Time,2); h=par2D(:,:,2); r0=1; mr=1; mrpp=100;
freq_th=[0 10];
while mr==r0 | abs(mr-1)>.01
 mrp=abs(mr-1);
 if mrp>mrpp
  error('Cannot fit this')
 end
 if mr~=r0, mrpp=mrp; end
 mr=mr*r0; r0=mr;
 lf=8.98e-6*sqrt(par2D(:,:,3))/mr.*sqrt(1+3*7.52e5*(fradar/3e8)^2*par2D(:,:,4)./par2D(:,:,3)*mr^2);
 lf=find_plf_peak(s,h,hlim,lf,16,freq_th,1);
 peak_lf=lf{1}(:,2);
 r=peak_lf(il)./plf(ip); r2=r.*2;
 mr2=median(r2); sr2=std(r2);
 good=find(abs(r2-mr2)<=maxe*sr2);
 bad=find(abs(r2-mr2)>maxe*sr2);
 mr=median(r(good)); sr2=std(r2(good));
 if r0==1
  axs(3)=copyobj(axs(1),gcf)
  set(gcf,'currentaxes',axs(3))
  set(gca,'nextplot','add')
  plot(mean(p_time)+eps,plf,'+g',mean(Time),peak_lf,'+b')
  set(gca,'nextplot','replace')
  set(gcf,'currentaxes',axs(1),'colormap',1-gray)
  plm=plm/max(max(plm))*length(get(gcf,'colormap'));
  delete(get(gca,'children')), delete(get(gca,'ylabel'))
  for i=1:nfl
   surface(p_time(:,i),[sf 2*sf(end)-sf(end-1)]'-fres/2,[plm(:,i);plm(end,i)]*ones(1,2))
  end
  set(gca,'xtick',[],'ytick',[],'ticklength',[0 0])
  if local.matlabversion>=7, linkaxes(axs([1 3])), end
  set(gcf,'currentaxes',axs(2))
  pmax=ceil(max([plf;peak_lf(il(good))]));
  rx=[0 pmax];
  plot(plf(ip(good)),peak_lf(il(good)),'o',plf(ip(bad)),peak_lf(il(bad)),'o',rx,rx*mr,'-',rx,rx,'-')
  hy=ylabel('Calculated plasmaline peak (MHz)');
  hx=xlabel('Measured plasmaline peak (MHz)');
  set(hx,'color','green'), set(hy,'color','blue')
  pos=get(gca,'position'); pos(3)=.5;
  set(gca,'xlim',[0 pmax],'ylim',[0 pmax],'position',pos), axis square
  freq_th=freq_meas;
 end
end
mr2=(mr*r0)^2; sr2=sr2*mr2;
newMagic=Magic_const/mr2;
%Need some overshoot for Te changes
better_guess=newMagic*exp(-log(mr2)/15); % 15 OK for 22May04 ESR
text(pmax*1.04,pmax/2,sprintf('Density ratio=%.2f\\pm%.2f\nMagic const used=%g\n\nheight=%.0f-%.0f km\ngreen circles > %g\\sigma\n\nsuggested Magic const=%.2f',mr2,sr2,Magic_const,mean(hlim),maxe,better_guess),'horiz','left')
if abs(mr2-1)>.01
 fprintf('Try Magic_const=%.2f; (%.2f)\n',better_guess,newMagic)
end
%text(3,.2,sprintf('Try Magic const=%.2f (%.2f)',newMagic,better_guess))
ratio=[mr2 sr2 newMagic better_guess length(good)];
