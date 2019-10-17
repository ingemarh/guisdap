function mr2=calib_ne(F,alt,maxe,minel,folim,polen,fpplt,ipar)
%function mr2=calib_ne(F,alt,maxe,minel,folim,fpplt)
%calib_ne.m: Utility to check the analysis against the dynasonde
% GUISDAP v.8.3 04-05-27 Copyright EISCAT
%  Changed 20160408, MTR to specify ipar, the ionosonde data type.
% Inputs: F: 1 FoE, 2 FoF2 (default)
%         alt: Altitude range, default 90-140, 180-500 resp.
%         maxe: Maximum error allowed, default 1 (* std)
%         minel: Minimum elevation allowed, default 75
%         folim: Frequency range allowed, default [1 Inf]
%         polen: Maximum length of polynomial fit, default 7, NaN=no fit
%         fpplt: Indices to display the parabolic fits of plfs
%         ipar: Ionosonde data type, 1 for asymptotic fit to crit freq (default)
%               or 2 for fmfe or fmxf (peak pfreq from NeXtYZ)
% Output: mr2: [Density_ratio Error NoPointsUsed NewMagic_const]
global fpp_plot plf_polen
if nargin<8, ipar=[]; epar=[]; fpar=[];  end
if nargin<7, fpplt=[]; end
if nargin<6, polen=[]; end
if nargin<5, folim=[]; end
if nargin<4, minel=[]; end
if nargin<3, maxe=[]; end
if nargin<2, alt=[]; end
if nargin<1 | isempty(F), F=2; end
if isempty(alt)
 if F==1; alt=[90 140];
 else, alt=[180 500];
 end
end
if isempty(maxe), maxe=1; end
if isempty(minel), minel=75; end
if isempty(folim), folim=[1 30]; end
if isempty(ipar), ipar=1; end
if ipar == 1, epar='foE';, fpar='foF2';
else epar='fmxe';, fpar='fmxf';
end
fpp_plot=fpplt;
plf_polen=polen;
a=vizu('verbose',alt,'P1 AE');
global Time axs par1D DATA_PATH START_TIME END_TIME r_Magic_const name_ant
d=datevec(Time(1));
[dd,fo]=get_fo(Time(1),Time(end),name_ant,epar,fpar);
t=[]; f=[];
d=find(dd>datenum(START_TIME) & dd<datenum(END_TIME) & fo(:,F)>folim(1) & fo(:,F)<folim(2));
dd=dd(d); fo=fo(d,:);
if ~isempty(dd)
 set(gcf,'currentaxes',axs(1))
 hold on
 plot(dd,fo(:,F),'*g')
 hold off

 %find guisdap data for sounding time
 for i=find(isfinite(fo(:,F)))'
  d=find(Time(2,:)>dd(i) & Time(1,:)<dd(i)+225/86400 & isfinite(a)' & par1D(:,2)'>minel);
  if ~isempty(d)
   t=[t;dd(i)]; f=[f;[fo(i,F) mean(a(d))]];
  end
 end
end
if isempty(t)
 fprintf('Could not find any valid dynasond foF2 for these times\n')
 mr2=[];
else
 fgup=fullfile(DATA_PATH,'.gup');
 Magic_const=1;
 if ~isempty(r_Magic_const)
  Magic_const=r_Magic_const;
 elseif exist(fgup,'file')
  load(fgup,'-mat')
  for i=1:size(extra,1),eval(extra(i,:));end
 end
 set(gcf,'currentaxes',axs(2))
%ne=(f*1e6/8.98).^2;
 r=f(:,2)./f(:,1); r2=r.^2;
 mr2=median(r2); sr2=std(r2);
 bad=find(abs(r2-mr2)>maxe*sr2);
 good=find(abs(r2-mr2)<=maxe*sr2);
 mr2=mean(r2(good)); sr2=std(r2(good));
 nmc=Magic_const/mr2;
 rx=[0 ceil(max(col(f)))]; %rrx=((rx(1):.1:rx(2)));
 plot(f(good,1),f(good,2),'o',f(bad,1),f(bad,2),'o',rx,rx*sqrt(mr2),'-',rx,rx,'-');
 FO={'foE','foF2'};
 FD=FO;
 if ipar ~=1
   FD={'fmxE','fmxF'};
 end
 ylabel(sprintf('EISCAT %s (MHz)',char(FO(F))))
 xlabel(sprintf('Dynasonde %s (MHz)',char(FD(F))))
 text(rx(2)*1.05,rx(2)/2,...
   sprintf('Density ratio=%.2f\\pm%.2f\nMagic const used=%g\n\nheight=%d-%d km\nelev > %g\\circ\ndynasonde window=%g-%g MHz\ngreen circles > %g\\sigma\n\nsuggested Magic const=%.2f',...
   mr2,sr2,Magic_const,alt,minel,folim,maxe,nmc),'horiz','left','UserData','Results')
 axis square
 pos=get(gca,'position'); pos(3)=.5;
 set(gca,'xlim',rx,'ylim',rx,'position',pos)
 if abs(mr2-1)>.005
  fprintf('Try Magic_const=%.2f;\n',nmc)
 end
 mr2=[mr2 sr2 length(good) nmc];
end
