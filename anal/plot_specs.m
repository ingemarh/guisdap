function plot_specs(file,g,l)
% GUISDAP v8.5   07-01-27 Copyright EISCAT
%
% Altitude plot of the saved spectra from the analysis
%  enabled by 'analysis_savespec'
% Input: Result file name
%
% function plot_specs(file,gates)
if nargin<3, lg=[]; end
if nargin<2, g=[]; end
if nargin<1, file=[]; end
nf=1; nlev=128;
if isempty(file)
 global r_om r_spec p_om0 sc_angle r_h name_expr name_ant d_time r_freq
else
 if isnumeric(file)
  load(canon(num2str(file(1))))
  nf=length(file); ss=[]; tt=[];
 else
  load(canon(file,0))
 end
 sc_angle=r_SCangle*2; p_om0=r_om0; d_time=r_time;
end
if isempty(g), g=1:length(r_h); end
if isempty(l), l='log'; end
lg=length(g);
if lg>1
 r_h=r_h(g);
 dh=diff(r_h)/2;
 h=[r_h(1)-dh(1);r_h(2:end)-dh;r_h(end)+dh(end)];
end
clf, set(gcf,'DefaultsurfaceEdgeColor','none'), colormap(vizu('myb',nlev))
o2=ones(2,1);
for ff=1:nf
 if ff>1
  load(canon(num2str(file(ff)),0))
 end
 ig=0;
 if isempty(r_freq)
  freq=r_om*p_om0(1)*sin(sc_angle/2)/2/pi*1e-3;
  sp=asinh(1000*r_spec/2);
  for i=g
   ig=ig+1;
   jj=find(isfinite(r_spec(:,i)));
   f=freq(jj);
   df=diff(f)/2;
   f=[f(1)-df(1);f(2:end)-df;f(end)+df(end)]'; s=[sp(jj,i)' sp(jj(end),i)];
   if nf>1
    ss(ff,:,ig)=s; tt=[tt datenum(r_time)];
   elseif exist('h','var')
    surface(o2*f,h(i+[0 1]),o2*s)
   else
    plot(f,s)
   end
  end
  set(gca,'xlim',[freq(1) freq(end)])
 else
  for i=g
   ig=ig+1;
   if nf>1
    jj=1:size(r_spec,1);
   else
    jj=find(isfinite(r_spec(:,i)));
   end
   f=r_freq(jj,i)/1000;
   df=diff(f)/2;
   f=[f(1)-df(1);f(2:end)-df;f(end)+df(end)]'; s=[r_spec(jj,i)' r_spec(jj(end),i)];
   if nf>1
    ss(ff,:,ig)=s; tt=[tt datenum(r_time)];
   elseif exist('h','var')
    surface(o2*f,h(i+[0 1]),o2*s)
   else
    plot(f,s)
   end
  end
  set(gca,'xlim',[min(min(r_freq)) max(max(r_freq))]/1000)
 end
end
set(gca,'xgrid','on','ygrid','on','box','on','layer','top')
if nf>1
 s=ss;
 if strcmp(l,'log')
  s0=exp(log(max(max(max(ss)))))/nlev/nlev; ss(find(ss<s0))=s0;
 else
  ss(find(ss<0))=0;
 end
 for gg=1:lg
  subplot(length(g),1,lg-gg+1)
  if strcmp(l,'log')
   imagesc(mean(tt),[f f(end)],log(ss(:,:,gg))')
  else
   imagesc(mean(tt),[f f(end)],ss(:,:,gg)')
  end
  set(gca,'ydir','normal')
  datetick('x')
  ylabel('Frequency (kHz)')
  text('pos',[1 1],'string',sprintf('%.0f km',r_h(g(gg))),'units','normal','horiz','right','vertical','base')
 end
 xlabel('Time (UT)')
 subplot(lg,1,1)
 title(sprintf('%s@%s %s',name_expr,name_ant,datestr(tt(1,1),1)))
else
 xlabel('Frequency (kHz)'), ylabel('Altitude (km)')
 title(sprintf('%s@%s %s',name_expr,name_ant,datestr(d_time(2,:),31)))
 sminmax=caxis, caxis([0 sminmax(2)])
end
