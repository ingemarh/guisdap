% function [Edate,E,Ee,Epos,Vm,Vme,MLT,ilat]=efield(vfile,p,alt,lat,verbose)
% Copyright EISCAT 2008-02-28
% Plot velocity vectors
% Input: vfile	filename with geographical vectors
%	p	['tVg'] plot type:	t versus time
%					3 separate plots + errors
%					p polar plot
%					E plot electric field
%					V plot velocities
%					g geographical coords
%					m (local) magnetic coords
%	alt	[0 Inf] altitude range, km
%	verbose	[0]	Specify plot parameters:	0 autoscale
%							1 interactive
%							2 fixed values
%				[minlat maxV maxVerr scaleV stretchtime]
% See also VECTOR_VELOCITY VEL_WRAPPER
%
function [varargout]=efield(vfile,p,alt,lat,verbose)
if nargin<2, p=[]; end
if nargin<3, alt=[]; end
if nargin<4, lat=[]; end
if nargin<5, verbose=[]; end
if isempty(p), p='tVg'; end
if isempty(alt), alt=[0 Inf]; end
if isempty(lat), lat=[0 Inf]; end
if isempty(verbose)
 verbose=0;
elseif verbose(1)==2
 minlat=60; scale=500; maxv=5000; maxverr=500; St=600;
elseif verbose(1)~=1
 minlat=verbose(1); scale=verbose(4); maxv=verbose(2); maxverr=verbose(3); St=verbose(5);
end
load(vfile)
if isempty(Vdate)
 error('Empty data file')
end
degrad=pi/180;
np=size(Vdate,2);
%%%%
if [strfind(p,'m') strfind(p,'E')]
 E=zeros(np,2); Ee=E; Vm=zeros(np,3); Vme=Vm; 
 for i=1:np
  r_time=datevec(mean(Vdate(:,i)));
  B=geomag(Vpos(i,:)',r_time);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  [D,I,r]=cart2sph(B(2),B(1),-B(3));
  gtm=[cos(D) -sin(D) 0;sin(I)*sin(D) sin(I)*cos(D) cos(I);-cos(I)*sin(D) -cos(I)*cos(D) sin(I)]';
  Vm(i,:)=Vg(i,:)*gtm; %geomagnetic
  BB=[0 0 -norm(B(1:3))];
  Em=-cross(Vm(i,:),BB);
  E(i,:)=Em(1:2); %Along B = 0 always
  Vgvv=reshape(Vgv(i,[1 4 6 4 2 5 6 5 3]),3,3);
  Vmv=abs(Vgvv*gtm);
  Vme(i,:)=sqrt(diag(Vmv));
  Ee(i,:)=BB(3)*Vme(i,[2 1]);
 end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
d=find(Vpos(:,3)>alt(1) & Vpos(:,3)<alt(2) & Vpos(:,1)>lat(1) & Vpos(:,1)<lat(2));
MLT=[]; ilat=[];
if ~isempty(p)
 global path_GUP
 ifor='\Lambda';
 dir={'East_{\perp}' 'North_{\perp}' 'Up_{//}'};
 if strfind(p,'V')
  ew=2;
  yfor1='\bfv\rm_i'; yfor2='ms^{-1}';
  if strfind(p,'m')
   vp=Vm; vpe=Vme;
  elseif strfind(p,'g')
   vp=Vg; vpe=sqrt(Vgv(:,1:3));
   ifor='\circN';
   dir={'East_{GG}' 'North_{GG}' 'Up_{GG}'};
  end
 elseif strfind(p,'E')
  ew=1;
  vp=E*1000; vpe=Ee*1000;
  yfor1='\bfE\rm'; yfor2='mVm^{-1}';
  if verbose(1)>1
   scale=scale/50; maxv=maxv/50; maxverr=maxverr/50;
  end
 end
 START_TIME=datevec(floor(24*(Vdate(1)+1/86400))/24);
 END_TIME=datevec(ceil(24*(Vdate(end)-1/86400))/24);
 if verbose(1)==1
  START_TIME=minput('Start time',START_TIME);
  END_TIME=minput('  End time',END_TIME);
 end
 if verbose(1)<2
  maxv=norm(vp(d,:),1)/length(d);
  if verbose
   maxv=minput('Max value to plot',maxv);
  end
  maxverr=min([maxv norm(vpe(d,:),1)/length(d)]);
  if verbose
   maxverr=minput('Max error to plot',maxverr);
  end
 end
 d=d(find(max(vpe(d,:),[],2)<maxverr & max(abs(vp(d,:)),[],2)<maxv));
 if isempty(d)
  error('No data within the limits')
 end
 vd=mean(Vdate(:,d));
 if isempty(findobj(gcf,'userdata','logo'))
  ax=gca;
  tit=datestr(mean(vd),29);
  pos=get(gcf,'defaultaxespos'); xp=pos(1); yp=sum(pos([2 4]));
  axes('Position',[xp yp pos(3) 1-yp],'visible','off');
  text('Position',[0.5 0.5],... %'VerticalAlignment','bottom',...
    'Horizontal','center','FontSize',24,'FontWeight','bold',...
    'String','EISCAT Scientific Association','userdata','logo');
  %load(fullfile(path_GUP,'matfiles','logo'))
  %axes('Position',[xp/10 yp xp/2 1-yp]); plot(y,x,'.k')
  %axis square
  %hds=get(gca,'child'); set(hds,'markersize',1,'userdata','logo')
  %set(gca,'xlim',[0 202],'ylim',[0 202],'visible','off')
  axes('Position',[xp/10 yp xp/2 1-yp]); eiscatlogo(1,4.2)
  set(gca,'userdata','logo')
  set(gcf,'currentaxes',ax);
 end
 vdd=[Vdate(2,d(1)) Vdate(1,d(end))];
 tit=[datestr(vdd(2),'dd mmm yyyy')];
 if tit(1)=='0', tit(1)=[]; end
 if diff(floor(vdd))
  ii=diff(datevec(vdd));
  if ii(2)
   tit=[datestr(vdd(1),'dd mmm') ' - ' tit];
  else
   tit=[datestr(vdd(1),'dd') '-' tit];
  end
  if tit(1)=='0', tit(1)=[]; end
 end
 if strfind(p,'g')
  hgt=sprintf('%.0fkm',mean(Vpos(d,3)));
 else
  hgt=[];
 end
 if strfind(p,'t')
  xlim=[datenum(START_TIME) datenum(END_TIME)];
  tickform='HH:MM';
  np=size(vp,2);
  if verbose(1)<2
   St=norm(diff(Vdate(:,d)),1)/length(d);
   if verbose
    St=minput('Max stretch time',St*86400)/86400;
   end
  end
  vdt=Vdate(:,d); ddt=vdt(1,2:end)-vdt(2,1:end-1);
  dt=find(ddt<St);
  if dt, vdt(2,dt)=vdt(2,dt)+ddt(dt)/2; vdt(1,dt+1)=vdt(2,dt); end
  dt=find(ddt>=St);
  vdt=vdt(:);
  vpt=reshape(repmat(row(vp(d,:)),2,1),[],np);
  for i=2*fliplr(dt)
   vpt=[vpt(1:i,:);NaN*ones(1,np);vpt(i+1:end,:)];
   vdt=[vdt(1:i);NaN;vdt(i+1:end)];
  end
  if strfind(p,'3')
   for i=1:np
    subplot(np,1,i)
    plot(vdt,vpt(:,i),'-k', ...
     ones(2,1)*vd,ones(2,1)*vp(d,i)'+[1;-1]*vpe(d,i)','-k')
    set(gca,'XLim',xlim)
    ylabel([yfor1 ' ' dir{i} ' [' yfor2 ']'])
    set(gca,'xgrid','on','ygrid','on')
    datetick(gca,'x',tickform,'keeplimits')
    text('String',hgt,'Units','Normalized','Position',[1.025 0.5],'Rotation',-90,'Color','k')
   end
  else
   plot(vdt,vpt)
   set(gca,'XLim',xlim)
   ylabel([yfor1 ' [' yfor2 '] ' hgt])
   set(gca,'xgrid','on','ygrid','on')
   datetick(gca,'x',tickform,'keeplimits')
   color=get(gca,'ColorOrder');
   for i=1:np
    text('String',dir{i},'Units','Normalized','Position',[.98+i*.035 0.5],'Rotation',-90,'Color',color(i,:))
   end
  end
  xlabel([tit ' [UT]'])
 elseif strfind(p,'p')
  if verbose(1)<2
   minlat=10*floor(min(Vpos(d,1)/10));
   scale=norm(vp(d,1:2),1)/length(d); scale_m=10^floor(log10(scale));
   scale=round(scale/scale_m)*scale_m;
   if verbose
    minlat=minput('Min latitude to display',minlat);
    scale=minput('Scale units/degree',scale);
   end
  end
  minrlat=90-minlat;
  [xc,yc]=pol2cart((0:360)'*degrad,1);
  latr=(1:floor(minrlat/10))*10;
  tscx=[-1 -1 0 1;1 1 0 -1]*minrlat; tscy=[0 -1 -1 -1;0 1 1 1]*minrlat;
  if strfind(p,'g')
   lt=(rem(vd',1)+Vpos(d,2)/(15*24)-.25)*2*pi;
   lat=Vpos(d,1);
   tt='LT';
  else
   addpath(fullfile(path_GUP,'models','onera','matlab'))
   try
    [Lm,Lstar,Blocal,Bmin,J,MLT]=onera_desp_lib_make_lstar([],[],'rll',vd',1+Vpos(d,3)/6378.135,Vpos(d,1),Vpos(d,2));
    ilat=acos(sqrt(1../abs(Lm)))/degrad;
   catch %dirty workaround for old matlabs
    MLT=onera_desp_lib_get_mlt(vd',gg2gc(Vpos(d,:)));
    B=geomag(Vpos(d,:)',datevec(mean(vd)));
    dip=-asin(B(3,:)./sqrt(sum(B(1:3,:).^2)))';
    warning('GUISDAP:efield','Using modip instead of invariant latitude')
    ilat=real(asin(dip./sqrt(dip.^2+cos(Vpos(d,1)*degrad))))/degrad; %modip
   end
   lt=(MLT/12-.5)*pi;
   lat=ilat;
   tt='MLT';
  end
  [x0,y0]=pol2cart(lt,90-lat);
  V_NE=vp(d,[2 1])/scale;
  x1=-sum([cos(lt) sin(lt)].*V_NE,2);
  y1=sum([-sin(lt) cos(lt)].*V_NE,2);
  de=find(V_NE(:,ew)<=0); dw=find(V_NE(:,ew)>0);
  plot(x0,y0,'ok','MarkerSize',1), hold on
  axis square
  x0=x0-x1/2; y0=y0-y1/2;
  quiver(x0(de),y0(de),x1(de),y1(de),0,'r'), hold on
  quiver(x0(dw),y0(dw),x1(dw),y1(dw),0,'b')
  quiver(-5,0,10,0,0,'k')
  plot(xc*latr,yc*latr,'k:',tscx,tscy,'k:'), hold off
  for i=0:3
   text(minrlat*1.2*xc(46+i*90),minrlat*1.2*yc(46+i*90),sprintf('%02.0f%s',rem(i*6+9,24),tt),'horiz','center')
  end
  text(0,0,{yfor1 [num2str(10*scale) ' ' yfor2]},'horiz','center','vertical','middle')
  lims=minrlat*[-1 1];
  set(gca,'xlim',lims,'ylim',lims,'visible','off')
  for i=latr
   text(i,0,sprintf('%d',90-i),'horiz','center')
  end
  text(minrlat,0,['  ' ifor])
  if hgt
   tit=[tit ' Height=' hgt];
  end
  text(0,-minrlat,tit,'horiz','center','vertical','top')
 end
end
if nargout>0
 if [strfind(p,'m') strfind(p,'E')]
  varargout={Vdate(:,d),E(d,:),Ee(d,:),Vpos(d,:),Vm(d,:),Vme(d,:),MLT,ilat};
 end
end
