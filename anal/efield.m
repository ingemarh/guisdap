% function efield(vfile,add)
% Copyright EISCAT 2008-02-28
% Rotate velocity vectors in 'vfile' and calculate perpendicular
%  electric fields
% See also VECTOR_VELOCITY
%
function [varargout]=efield(vfile,p,add)
if nargin<2, p=[]; end
if nargin<3, add=[]; end
if isempty(p), p=1; end
if isempty(add), add=0; end
global path_GUP
load(vfile)
degrad=pi/180;
np=size(Vdate,2);
E=zeros(np,2); Ee=E; Vm=zeros(np,3); Vme=Vm; 
for i=1:size(Vdate,2)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end
if add
 save(vfile,'Vdate','Vpos','Vg','Vgv','E','Ee')
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% load Efield_CP2_2005-09-12_tau2pl_0@uhf.mat
if p
 START_TIME=floor(datevec(Vdate(1)));
 END_TIME=ceil(datevec(Vdate(end)));
 START_TIME=minput('Start time',START_TIME);
 END_TIME=minput('  End time',END_TIME);
 if p==1
  xlim=[datenum(START_TIME) datenum(END_TIME)];
  fa=find(max(abs(E),[],2)<0.1);
  tickform='HH:MM';
  d={'east' 'north'};
  for i=1:2
   subplot(2,1,i)
   plot(mean(Vdate(:,fa)),E(fa,i)*1000,'-ok')
   hold on
   plot(ones(2,1)*mean(Vdate(:,fa)),1000*(ones(2,1)*E(fa,i)'+[1;-1]*Ee(fa,i)'),'-k')
   hold off
   set(gca,'XLim',xlim,'ylim',[-100 100])
   title(datestr(mean(mean(Vdate)))),xlabel('UT'),ylabel(['E ' d{i} ' [mVm^{-1}]'])
   set(gca,'xgrid','on','ygrid','on')
   datetick(gca,'x',tickform,'keeplimits')
  end
 elseif p==2
  scale=1000; %m/s /degree
  minlat=60;
  maxerr=300; maxv=5000;
  d=find(max(Vme,[],2)<maxerr & max(abs(Vm),[],2)<maxv);
  addpath(fullfile(path_GUP,'models','onera','matlab'))
  [Lm,Lstar,Blocal,Bmin,J,MLT]=onera_desp_lib_make_lstar([],[],'rll',mean(Vdate(:,d))',1+Vpos(d,3)/6378.135,Vpos(d,1),Vpos(d,2));
  lt=(MLT/12-.5)*pi;
  ilat=180/pi*acos(sqrt(1../abs(Lm)));
  [x0,y0]=pol2cart(lt,90-ilat);
  %plot(x0,y0,'o')
  V_NE=Vm(d,[2 1])/scale;
  x1=[x0 x0-sum([cos(lt) sin(lt)].*V_NE,2)]';
  y1=[y0 y0+sum([-sin(lt) cos(lt)].*V_NE,2)]';
  de=find(V_NE(:,2)>0); dw=find(V_NE(:,2)<0);
  plot(x1(:,de),y1(:,de,:),'r-',x1(:,dw),y1(:,dw),'b-',x0(de),y0(de),'r.',x0(dw),y0(dw),'b.')
  lims=(90-minlat)*[-1 1]; set(gca,'xlim',lims,'ylim',lims)
  axis square
  title(sprintf('%s %d ms^{-1}/deg InvLat>%d',datestr(mean(mean(Vdate))),scale,minlat))
 end
end
