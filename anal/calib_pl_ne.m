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
if strcmp(expt,'steffe') | (isempty(expt) & strfind(pl_dir,'steffe'))
 nfft=128; nint=2; ngates=1;
 freq=[-4 -5.3 4 5.3]*1e6; dt=0.6e-6; invert=-1;
 ran=[182 423.9]; fradar=500e6;
 maxe=2; ele=81.6; updown=0:1; nup_d=2;
 startad=1; gate=1; skip_if=0;
elseif strcmp(expt,'tau8') | (isempty(expt) & strfind(pl_dir,'tau8'))
 nfft=128; nint=2; ngates=4;
 freq=4.1*1e6; dt=0.6e-6; invert=-1;
 ran=[53.3 272.4;193.4 412.5;333.4 552.5;473.3 692.4]; fradar=224e6;
 maxe=2; ele=0; updown=0; nup_d=1;
 startad=108679; skip_if=1;
 if isempty(gate), gate=2; end
else
 error('No such experiment defined')
end
if isempty(gate), gate=1; end

%pl_dir='/home/ingemar/gup/mydata/steffel_int_CP@32p/';
%res_dir='gup/results/2004-05-2*_steffe_60@42m/';
%assume similar integration!
global Time par1D par2D axs r_Magic_const DATA_PATH
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
 Magic_const=r_Magic_const;
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
plspec=zeros(nfft,nfreq,nfl);
p_time=zeros(2,nfl);
for i=1:nfl
 filename=fullfile(fl(i).dir,sprintf('%08d%s',fl(i).file,fl(i).ext));
 load(canon(filename,0))
 p_time(:,i)=datenum(d_parbl(1:6)')+[-d_parbl(7)/86400;0];
 d=sum(reshape(real(d_data(startad:end)),nfft,ngates,nint,nfreq),3)/d_parbl(22);
 plspec(:,:,i)=reshape(d([nfft/2+1:nfft 1:nfft/2],gate,1,:),nfft,nfreq);
end
if plots
 figure(9)
 imagesc(reshape(plspec,nfft*nfreq,[])), pause
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
 plot([filt_shape spec_shape]), pause
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
 imagesc(reshape(plsig,nfft*nfreq,[])), pause
end
plsig=reshape(plsig,nfft*nfreq,[])./(filt_shape(:)*ones(1,nfl));
plsig=reshape(plsig,nfft,nfreq,[]);

%Find frequencies of the maximum point
freq_scale=invert*(-nfft/2:nfft/2-1)/dt/nfft;
df=abs(diff(freq_scale(1:2)));
if plots
 plot(freq_scale'*ones(1,nfreq)+ones(nfft,1)*freq,plsig(:,:,round(nfl/2))), pause
end
freq_meas=10e-6*abs([freq_scale(1)+freq freq_scale(end)+freq]);
freq_meas=[min(floor(freq_meas)) max(ceil(freq_meas))]/10;

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
 plot([abs(plpeak_c)/1e6 plf]), pause
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
pmax=ceil(max(plf)); freq_th=[0 10];
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
  set(gcf,'currentaxes',axs(1))
  hold on
  plot(mean(p_time),plf,'+g',mean(Time),peak_lf,'+b')
  hold off
  set(gcf,'currentaxes',axs(2))
  rx=[0 pmax];
  plot(plf(ip(good)),peak_lf(il(good)),'o',plf(ip(bad)),peak_lf(il(bad)),'o',rx,rx*mr,'-',rx,rx,'-')
  hy=ylabel(sprintf('Calculated plasmaline peak (MHz)  Magic const=%g',Magic_const));
  hx=xlabel('Measured plasmaline peak (MHz)');
  set(hx,'color','green'), set(hy,'color','blue')
  set(gca,'xlim',[0 pmax],'ylim',[0 pmax]), axis square
  freq_th=freq_meas;
 end
end
mr2=(mr*r0)^2; sr2=sr2*mr2;
text(3,.5,sprintf('Density ratio=%.2f (%.2f)',mr2,sr2))
newMagic=Magic_const/mr2;
%Need some overshoot for Te changes
better_guess=newMagic*exp(-log(mr2)/15); % 15 OK for 22May04 ESR
if abs(mr2-1)>.01
 fprintf('Try Magic_const=%.2f; (%.2f)\n',better_guess,newMagic)
end
ratio=[mr2 sr2 newMagic better_guess length(good)];
