function ratio=calib_pl_ne(pl_dir,expt,plots)
if nargin<2, expt=[]; end
if nargin<3, plots=[]; end
if strcmp(expt,'steffe') | (isempty(expt) & strfind(pl_dir,'steffe'))
 nfft=128; nfreq=4; nint=2;
 freq=[-4 -5.3 4 5.3]*1e6; dt=0.6e-6;
 ran=[182 423.9]; fradar=500e6;
 maxe=2; ele=81.6;
else
 error('No such experiement defined')
end

%pl_dir='/home/ingemar/gup/mydata/steffel_int_CP@32p/';
%res_dir='gup/results/2004-05-2*_steffe_60@42m/';
%assume similar integration!
global Time par1D par2D axs r_Magic_const DATA_PATH

%First guess of altitudes (Not very important)
re=6370; ratio=[];
alt=re*sqrt(1+ran/re.*(ran/re+2*sin(ele/57.2957795)))-re;
%Read in the analysed data
lf=vizu('verbose',alt,'L1 AE');
for i=1:2
 hlim(:,i)=ran(i)*(ran(i)/re+2*sin(par1D(:,2)/57.2957795));
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
 d=reshape(sum(reshape(real(d_data),nfft,nint,nfreq),2),nfft,nfreq)/d_parbl(22);
 plspec(:,:,i)=d([nfft/2+1:nfft 1:nfft/2],:);
end
if plots
 figure(9)
 imagesc(reshape(plspec,nfft*nfreq,[])), pause
end

%Get spectral shape and find interferences
disp('Removing noise')
spec_shape=median(plspec,3);
filt_shape=spec_shape;
fsh=[filt_shape(end-8:end,:);filt_shape;filt_shape(1:9,:)];
d2=diff(filt_shape,2);
fd2=-std(d2);
for i=1:nfreq
 d=find(d2(:,i)<2*fd2(i));
 for j=d'
  filt_shape(j+(1:3),i)=interp1([-3:-1 5:7]',fsh(j+9+[-3:-1 5:7],i),(1:3)','spline');
 end
end
if plots
 plot([filt_shape spec_shape]), pause
end
interf_spec=spec_shape-filt_shape;

%Find a timevarying background (Use Tsys instead?)
fp=reshape(median(plspec,1),nfreq,[]);
fr=mean(spec_shape,1)./median(spec_shape,1);
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
freq_scale=-(-nfft/2:nfft/2-1)/dt/nfft; % inverted on plasmabox
df=-diff(freq_scale(1:2));
if plots
 plot(freq_scale'*ones(1,nfreq)+ones(nfft,1)*freq,plsig(:,:,330)), pause
end
plpeak=ones(2,nfl,nfreq)*NaN;
s=std(plsig(:));
for j=1:nfreq
 for i=1:nfl
  [a,b]=max(plsig(:,j,i));
  if a>s & b>10 & b<nfft-9
   plpeak(:,i,j)=[freq_scale(b)+freq(j);a];
  end
 end
end
if isempty(find(isfinite(plpeak)))
 error('No plasma lines found')
end
plpeak_c=ones(nfl,2)*NaN;
for i=0:1
 [a,b]=max(plpeak(2,:,(1:nfreq/2)+i*nfreq/2),[],3);
 for j=1:nfl
  plpeak_c(j,i+1)=plpeak(1,j,b(j)+i*nfreq/2);
 end
end
plf=ones(nfl,1)*NaN;
d=find(abs(sum(plpeak_c,2))<5*df);
if isempty(d)
 error('No simultanous up- and downshifted plasma lines found')
end
plf(d)=mean(abs(plpeak_c(d,:)),2)/1e6;
if plots
 plot([abs(plpeak_c) plf]), pause
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
s=size(Time,2); h=par2D(:,:,2); r0=1; mr=1;
pmax=ceil(max(plf));
while mr==r0 | abs(mr-1)>.01
 mr=mr*r0; r0=mr;
 lf=8.98e-6*sqrt(par2D(:,:,3))/mr.*sqrt(1+3*7.52e5*(fradar/3e8)^2*par2D(:,:,4)./par2D(:,:,3)*mr^2);
 lf=find_plf_peak(s,h,hlim,lf,16,[0 10],1);
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
 end
end
mr2=(mr*r0)^2; sr2=sr2*mr2;
text(3,.5,sprintf('Density ratio=%.2f (%.2f)',mr2,sr2))
newMagic=Magic_const/mr2;
%Need some overshoot for Te changes
better_guess=newMagic*exp(-log(mr2)/15); % 15 OK for 22May04
if abs(mr2-1)>.01
 fprintf('Try Magic_const=%.2f; (%.2f)\n',better_guess,newMagic)
end
ratio=[mr2 sr2 newMagic better_guess];
