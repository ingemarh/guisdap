function [varargout]=plasma_summary(pl_dir,expt,gate,plots)
% function plasma_summary(pl_dir,expt,gate,plots)
% GUISDAP 8.6  8 Oct 2010  Copyright EISCAT
% Get/plot summary for plasmalines
% Using setup in pl_def.m in experiment directory
% input: pl_dir Directory containing integrated plasma lines (needed)
%        expt   Name of experiment (or guessed from pl_dir)
%        plots  Display a number of plots for internal checks
% output: pl containing
%            .t Time
%            .s Spectra
%            .f Freqeuencies
if nargin<2, expt=[]; end
verbose=0;
if nargin<3
 gate=[];
elseif gate==0
 gate=[]; verbose=1;
end
if nargin<4, plots=0; end
if isempty(expt)
 [i,expt]=fileparts(pl_dir);
 if isempty(expt), [i,expt]=fileparts(i); end
end
global path_exps path_GUP
exps=dir(path_exps); pldef='pl_def'; pdefs=[];
for i=1:length(exps)
 d=fullfile(path_exps,exps(i).name,pldef);
 if exist([d '.m'],'file')
  pdefs=[pdefs i];
  if strfind(expt,exps(i).name) & exist([d '.m'],'file')
   run(d),break
  end
 end
end
if ~exist('startad','var')
 pdefs=sprintf('%s ',exps(pdefs).name);
 warning(['No such experiment defined ( ' pdefs ')'])
 verbose=1;
 ran=[200 300]; nfft=128; nint=1; ngates=1; nlag=0; maxe=2; nup_d=1; skip_if=0;
 freq=[-5 5]*1e6; dt=1e-6; invert=1; fradar=930e6; ele=77.5; updown=0:1;
 startad=0;
end
if isempty(gate), gate=1; end
if verbose
 ran=minput('Ranges (km)',ran);
 nfft=minput('No fft points',nfft);
 nint=minput('No to integrate',nint);
 ngates=minput('No of gates',ngates);
 nlag=minput('No of lags',nlag);
 maxe=minput('Max sigma',maxe);
 nup_d=minput('No frequencies',nup_d);
 skip_if=minput('Skip interference',skip_if);
 freq=minput('Offset frequencies',freq);
 dt=minput('Sampling frequency',dt);
 invert=minput('Invert frequency scale',invert);
 fradar=minput('Radar frequency',fradar);
 ele=minput('Approx elevation',ele);
 updown=minput('Up/Down shifted (0/1)',updown);
 startad=minput('Start address',startad);
 gate=minput('Gate',gate);
end
%pl_dir='/home/ingemar/gup/mydata/steffel_int_CP@32p';
%res_dir='gup/results/2004-05-2*_steffe_60@42m/';
%assume similar integration!
nfreq=length(freq);

%Get plasmaline data
[fl,msg]=getfilelist(fullfile(pl_dir,filesep));
if msg, error(msg), end
d=cell2mat({fl.file});
nfl=length(fl);
endad=startad+(nfft+nlag)*ngates*nint*nfreq/length(startad)-1;
add=[]; for i=1:length(startad), add=[add startad(i):endad(i)]; end
if nlag>0, nfft=2*nlag; end
plspec=zeros(nfft,nfreq,nfl);
p_time=zeros(2,nfl);
upar=zeros(nfreq,nfl);
for i=1:nfl
 filename=fullfile(fl(i).dir,sprintf('%08d%s',fl(i).file,fl(i).ext));
 load(canon(filename,0))
 p_time(:,i)=datenum(row(d_parbl(1:6)))+[-d_parbl(7)/86400;0];
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
 if exist('uparfreq','var')
  upar(:,i)=d_parbl(46+(1:nfreq));
 end
end
if plots>1
 imagesc(reshape(plspec,nfft*nfreq,[])), pause
end
plsig=zeros(size(plspec));

bands=unique(upar(1,:));
for band=1:length(bands)
 b=find(upar(1,:)==bands(band));
 %Get spectral shape and find interferences
 disp('Removing noise')
 wrap=9;
 spec_shape=median(plspec(:,:,b),3);
 filt_shape=spec_shape;
 fsh=[filt_shape(end-wrap+1:end,:);filt_shape;filt_shape(1:wrap,:)];
 d2=diff(filt_shape,2);
 fd2=-std(d2);
 for i=1:nfreq
  d=find(d2(:,i)<2*fd2(i));
  for j=d'
   jj=3; if j>nfft-3, jj=2; end
   filt_shape(j+(1:jj),i)=interp1([-3:-1 5:7]',fsh(j+wrap+[-3:-1 5:7],i),(1:jj)','spline');
  end
 end
 if plots>1
  plot([filt_shape spec_shape]), pause
 end
 interf_spec=spec_shape-filt_shape;
 if skip_if
  interf_spec(find(interf_spec~=0))=NaN;
 %filt_shape=spec_shape;
 end
 
 %Find a timevarying background (Use Tsys instead?)
 fp=reshape(median(plspec(:,:,b),1),nfreq,[]);
 nplspec=zeros(size(plspec(:,:,b)));
 spnorm=median(filt_shape,1);
 for i=1:nfreq
  nplspec(:,i,:)=(filt_shape(:,i)*fp(i,:)/spnorm(i));
 end
 %Add interference lines to background
 nplspec=reshape(reshape(nplspec,nfft*nfreq,[])+interf_spec(:)*ones(1,length(b)),nfft,nfreq,[]);
 
 %Remove background and normalise signal
 plsig(:,:,b)=plspec(:,:,b)-nplspec;
 if plots>1
  imagesc(reshape(plsig(:,:,b),nfft*nfreq,[])), pause
 end
end
 
freq_scale=invert*(-nfft/2:nfft/2-1)/dt/nfft;
if exist('uparfreq','var')
 if isnan(uparfreq(1))
  freq=1e6*upar;
 else
  freq=(uparfreq'*ones(1,nfl)+invert*upar)*1e6;
 end
else
 freq=col(freq)*ones(1,nfl);
end
df=diff(freq_scale(1:2));
plfreq=reshape(freq_scale'*ones(1,nfreq*nfl)+ones(nfft,1)*reshape(freq,1,[]),nfft,nfreq,[]);
if nargout
 varargout(1)={struct('t',p_time,'f',plfreq,'s',plsig)};
 if nargout>1
  varargout(2)={struct('ran',ran(gate,:),'ele',ele,'updown',updown,'nup_d',nup_d,'df',df,'fradar',fradar,'maxe',maxe)};
 end
end
%plsig(find(plsig<1))=1; plsig=log(plsig);
if plots
 if plots==1
  mmmsig=max(max(max(plsig)));
  set(gcf,'Position',[200 30 587 807],'renderer','painters','PaperPosition',[0.4 0.7 20.65 28.4],...
  'DefaultAxesFontName','Helvetica','DefaultTextFontName','Helvetica',...
  'DefaultAxesFontSize',14,'DefaultTextFontSize',14)
  for j=1:nfreq
   plfreqj=reshape(([plfreq(:,j,:);plfreq(end,j,:)+df]-df/2)/1e6,nfft+1,[]);
   %subplot(nfreq,1,nfreq-j+1,'align')
   axes('position',[.12 (j-1)*.85/nfreq+.1 .85 .75/nfreq])
   for i=1:nfl
    surface(p_time(:,i),plfreqj(:,i),[plsig(:,j,i);plsig(end,j,i)]*ones(1,2))
   end
   %pcolor(p_time(1,:),(plfreq(:,i)-df/2)/1e6,reshape(plsig(:,i,:),nfft,[]))
   shading flat, caxis([0 mmmsig])
   datetick, ylabel('Frequency offset[MHz]')
   if j==1, xlabel('Time [UT]'), end
   set(gca,'xlim',p_time([1 end]),'ylim',[min(min(plfreqj)) max(max(plfreqj))],'box','on','tickdir','out')
  end
  [h,d]=strtok(d_ExpInfo);
  h=title(sprintf('Plasma lines  %s  %s',d,datestr(mean(p_time(1,:)),1)));
  set(h,'interpreter','none')
  colormap(myb)
  load(fullfile(path_GUP,'matfiles','logo'))
  axes('Position',[.07 .92 .08 .06]); plot(y,x,'.k')
  set(get(gca,'child'),'markersize',1)
  set(gca,'xlim',[0 202],'ylim',[0 202],'visible','off')
  text('Position',[220,200],'VerticalAlignment','top','FontSize',16,...
      'FontWeight','bold','String','EISCAT Scientific Association');
 else
  plot(plfreq,plsig(:,:,round(nfl/2)))
  pause
 end
end
