% function efield(vfile,p,alt,verbose)
% Copyright EISCAT 2008-02-28
% Plot velocity vectors
% Input: vfile	filename with geographical vectors
%	p	['tE'] plot type:	t versus time
%					3 separate plots + errors
%					p polar plot
%					E plot electric field
%					V plot velocities
%					g geographical coords
%					m magnetic coords
%	alt	[0 Inf] altitude range, km
%	verbose	[0]	Specify some plot parameters
% See also VECTOR_VELOCITY
%
function [varargout]=efield(vfile,p,alt,verbose)
if nargin<2, p=[]; end
if nargin<3, alt=[]; end
if nargin<4, verbose=[]; end
if isempty(p), p='tE'; end
if isempty(alt), alt=[0 Inf]; end
if isempty(verbose), verbose=0; end
global path_GUP
load(vfile)
degrad=pi/180;
np=size(Vdate,2);
%%%%
minlatD=90-60; scaleV=1000; scaleE=90;
maxV=4500; maxVe=300; maxE=100; maxEe=10;
%%%%
if [strfind(p,'m') strfind(p,'E')]
 E=zeros(np,2); Ee=E; Vm=zeros(np,3); Vme=Vm; 
 for i=1:np
  r_time=datevec(mean(Vdate(:,i)));
  B=geomag(Vpos(i,:)',r_time);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %Eg=-cross(Vg(i,:),B); % V/m geographic
  [D,I,r]=cart2sph(B(2),B(1),-B(3));
  gtm=[cos(D) -sin(D) 0;sin(I)*sin(D) sin(I)*cos(D) cos(I);-cos(I)*sin(D) -cos(I)*cos(D) sin(I)]';
  Vm(i,:)=Vg(i,:)*gtm; %geomagnetic
  %Em=Eg*gtm, % geomagnetic
  BB=[0 0 -norm(B(1:3))];
  Em=-cross(Vm(i,:),BB);
  E(i,:)=Em(1:2); %Along B = 0 always
  Vgvv=reshape(Vgv(i,[1 4 6 4 2 5 6 5 3]),3,3);
  Vmv=Vgvv*abs(gtm);
  Vme(i,:)=sqrt(diag(Vmv));
  EE=abs(cross(Vme(i,:),BB));
  Ee(i,:)=EE(1:2);
 end
 %save(vfile,'Vdate','Vpos','Vg','Vgv','E','Ee')
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if ~isempty(p)
 ifor='\Lambda';
 if strfind(p,'V')
  if strfind(p,'m')
   vp=Vm; vpe=Vme;
   yfor='V_m %s [ms^{-1}]%s';
  elseif strfind(p,'g')
   vp=Vg; vpe=sqrt(Vgv(:,1:3));
   yfor='V_g %s [ms^{-1}]%s';
   ifor='\circN';
  end
 elseif strfind(p,'E')
  vp=E*1000; vpe=Ee*1000;
  yfor='E %s [mVm^{-1}]%s';
 end
 START_TIME=floor(datevec(Vdate(1)));
 END_TIME=ceil(datevec(Vdate(end)));
 if verbose
  START_TIME=minput('Start time',START_TIME);
  END_TIME=minput('  End time',END_TIME);
 end
 d=find(Vpos(:,3)>alt(1) & Vpos(:,3)<alt(2));
 maxv=norm(vp(d,:),1)/length(d);
 if verbose
  maxv=minput('Max value to plot',maxv);
 end
 maxverr=min([maxv norm(vpe(d,:),1)/length(d)]);
 if verbose
  maxverr=minput('Max error to plot',maxverr);
 end
 d=d(find(max(vpe(d,:),[],2)<maxverr & max(abs(vp(d,:)),[],2)<maxv));
 vd=mean(Vdate(:,d));
 if isempty(findobj(gcf,'userdata','logo'))
  ax=gca;
  tit=datestr(mean(vd),29);
  pos=get(gcf,'defaultaxespos'); xp=pos(1); yp=sum(pos([2 4]));
  axes('Position',[xp yp pos(3) 1-yp],'visible','off');
  text('Position',[0.5 0.5],... %'VerticalAlignment','bottom',...
    'Horizontal','center','FontSize',24,'FontWeight','bold',...
    'String','EISCAT Scientific Association','userdata','logo');
  load(fullfile(path_GUP,'matfiles','logo'))
  axes('Position',[xp/10 yp xp/2 1-yp]); plot(y,x,'.k')
  axis square
  hds=get(gca,'child'); set(hds,'markersize',1,'userdata','logo')
  set(gca,'xlim',[0 202],'ylim',[0 202],'visible','off')
  set(gcf,'currentaxes',ax);
 end
 vdd=[Vdate(2,d(1)) Vdate(1,d(end))];
 tit=[datestr(vdd(2),'dd mmm yyyy')];
 if diff(floor(vdd))
  ii=diff(datevec(vdd));
  if ii(2)
   tit=[datestr(vdd(1),'dd mmm') ' - ' tit]
  else
   tit=[datestr(vdd(1),'dd') '-' tit]
  end
 end
 if strfind(p,'g')
  hgt=sprintf(' %.0fkm',mean(Vpos(d,3)));
 else
  hgt=[];
 end
 if strfind(p,'t')
  xlim=[datenum(START_TIME) datenum(END_TIME)];
  tickform='HH:MM';
  dir={'east' 'north' 'up'};
  np=size(vp,2);
  St=norm(diff(Vdate(:,d)),1)/length(d);
  if verbose
   maxverr=minput('Max stretch time',St*86400)/86400;
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
    ylabel(sprintf(yfor,dir{i},[]))
    set(gca,'xgrid','on','ygrid','on')
    datetick(gca,'x',tickform,'keeplimits')
    if hgt, legend(hgt), end
   end
  else
   plot(vdt,vpt)
   set(gca,'XLim',xlim)
   ylabel(sprintf(yfor,[],hgt))
   set(gca,'xgrid','on','ygrid','on')
   datetick(gca,'x',tickform,'keeplimits')
   legend(dir(1:np))
  end
  xlabel([tit ' [UT]'])
 elseif strfind(p,'p')
  minlat=10*floor(min(Vpos(d,1)/10));
  if verbose
   minlat=minput('Min latitude to display',minlat);
  end
  minrlat=90-minlat;
  [xc,yc]=pol2cart((0:360)'/180*pi,1);
  latr=(1:floor(minrlat/10))*10;
  tscx=[-1 -1 0 1;1 1 0 -1]*minrlat; tscy=[0 -1 -1 -1;0 1 1 1]*minrlat;
  if strfind(p,'g')
   lt=(rem(vd',1)+Vpos(d,2)/(15*24)-.25)*2*pi;
   lat=Vpos(d,1);
   tt='LT';
  else
   addpath(fullfile(path_GUP,'models','onera','matlab'))
   [Lm,Lstar,Blocal,Bmin,J,MLT]=onera_desp_lib_make_lstar([],[],'rll',vd',1+Vpos(d,3)/6378.135,Vpos(d,1),Vpos(d,2));
   lt=(MLT/12-.5)*pi;
   lat=180/pi*acos(sqrt(1../abs(Lm)));
   tt='MLT';
  end
  [x0,y0]=pol2cart(lt,90-lat);
  %plot(x0,y0,'o')
  scale=10^(round(log10(norm(vp(d,1:2),1)/length(d))));
  if verbose
   scale=minput('Scale units/degree',scale);
  end
  V_NE=vp(d,[2 1])/scale;
  x1=-sum([cos(lt) sin(lt)].*V_NE,2);
  y1=sum([-sin(lt) cos(lt)].*V_NE,2);
  de=find(V_NE(:,2)>0); dw=find(V_NE(:,2)<0);
  quiver(x0(de),y0(de),x1(de),y1(de),0,'r'), hold on
  quiver(x0(dw),y0(dw),x1(dw),y1(dw),0,'b')
  quiver(-5,-minrlat/50,10,0,0,'k')
  plot(xc*latr,yc*latr,'k:',tscx,tscy,'k:'), hold off
  for i=0:3
   text(minrlat*1.2*xc(46+i*90),minrlat*1.2*yc(46+i*90),sprintf('%02.0f%s',rem(i*6+9,24),tt),'horiz','center')
  end
  text(0,0,sprintf(yfor,num2str(10*scale),[]),'horiz','center','vertical','bottom')
  lims=minrlat*[-1 1];
  set(gca,'xlim',lims,'ylim',lims,'visible','off')
  axis square
  for i=latr
   text(i,0,sprintf('%d',90-i),'horiz','center')
  end
  text(minrlat,0,['  ' ifor])
  if hgt
   tit=[tit ' Height =' hgt];
  end
  text(0,-minrlat,tit,'horiz','center','vertical','top')
 end
end
