function plot_specs(file)
% GUISDAP v8.5   07-01-27 Copyright EISCAT
%
% Altitude plot of the saved spectra from the analysis
%  enabled by 'analysis_savespec'
% Input: Result file name
%
% function plot_specs(file)
if nargin<1
 global r_om r_spec p_om0 sc_angle r_h name_expr name_ant d_time r_freq
else
 load(canon(file,0))
 sc_angle=r_SCangle*2; p_om0=r_om0; d_time=r_time;
end
dh=diff(r_h)/2;
clf, set(gcf,'DefaultsurfaceEdgeColor','none')
h=[r_h(1)-dh(1);r_h(1:end-1)+dh;r_h(end)+dh(end)];
o2=ones(2,1);
if isempty(r_freq)
 freq=r_om*p_om0(1)*sin(sc_angle/2)/2/pi*1e-3;
 sp=asinh(1000*r_spec/2);
 for i=1:length(r_h)
  jj=find(isfinite(r_spec(:,i)));
  f=freq(jj);
  df=diff(f)/2;
  f=[f(1)-df(1);f(2:end)-df;f(end)+df(end)]'; s=[sp(jj,i)' sp(jj(end),i)];
  surface(o2*f,h(i+[0:1]),o2*s)
 end
 set(gca,'xlim',[freq(1) freq(end)])
else
 for i=1:length(r_h)
  jj=find(isfinite(r_spec(:,i)));
  f=r_freq(jj,i)/1000;
  df=diff(f)/2;
  f=[f(1)-df(1);f(2:end)-df;f(end)+df(end)]'; s=[r_spec(jj,i)' r_spec(jj(end),i)];
  surface(o2*f,h(i+[0:1]),o2*s)
 end
 set(gca,'xlim',[min(min(r_freq)) max(max(r_freq))]/1000)
end
set(gca,'xgrid','on','ygrid','on','box','on','layer','top')
xlabel('Frequency (kHz)'), ylabel('Altitude (km)')
title(sprintf('%s@%s %s',name_expr,name_ant,datestr(d_time(2,:),31)))
