function mr2=calib_ne(F,alt,maxe,minel)
%function mr2=calib_ne(F,alt,maxe,minel)
%calib_ne.m: Utility to check the analysis against the dynasonde
% GUISDAP v.8.3 04-05-27 Copyright EISCAT
% Inputs: F: 1 FoE, 2 FoF2 (default)
%         alt: Altitude range, default 90-140, 180-500 resp.
%         maxe: Maximum error allowed, default 1 (* std)
%         minel: Minimum elevation allowed, default 75
% Output: mr2: [Density_ratio Error NewMagic_const]
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
a=vizu('verbose',alt,'P1 AE');
global Time axs par1D DATA_PATH START_TIME END_TIME r_Magic_const
d=datevec(Time(1));
[dd,fo]=get_fo(d(1),d(2));
t=[]; f=[];
d=find(dd>datenum(START_TIME) & dd<datenum(END_TIME));
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
if ~isempty(t)
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
 rx=[0 ceil(max(col(f)))]; %rrx=((rx(1):.1:rx(2)));
 plot(f(good,1),f(good,2),'o',f(bad,1),f(bad,2),'o',rx,rx*sqrt(mr2),'-',rx,rx,'-');
 FO={'foE','foF2'};
 ylabel(sprintf('EISCAT %s (MHz)  Magic const=%g',char(FO(F)),Magic_const))
 xlabel(sprintf('Dynasond %s (MHz)',char(FO(F))))
 text(3,1,sprintf('Density ratio=%.2f (%.2f)',mr2,sr2))
 set(gca,'xlim',rx,'ylim',rx)
 axis square
 if abs(mr2-1)>.005
  fprintf('Try Magic_const=%.2f;\n',Magic_const/mr2)
 end
else
 fprintf('Could not find any valid dynasond foF2 for these times\n')
end
mr2=[mr2 sr2 Magic_const/mr2];
