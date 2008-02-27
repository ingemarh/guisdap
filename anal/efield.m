function [varargout]=efield(vfile,add)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if nargin<2, add=[]; end
if isempty(add), add=0; end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load(vfile)
mexing=1;
if ~exist('geomag')
 mexing=0;
end
degrad=pi/180;
E=[]; Ee=[];
for i=1:size(Vdate,2)
 r_time=datevec(mean(Vdate(:,i)));
 if mexing
  bpar=geomag(Vpos(i,:),r_time);
  B=[bpar(1) -bpar(2) -bpar(3)]; %[N W U] Tesla
 else
  iday=(r_time(2)-1)*30+r_time(3);
  r=Vpos(i,3)/r_earth+1;
  theta=(90.-Vpos(i,1))*degrad;
  phi=Vpos(i,2)*degrad;
% BR, BTHETA, BPHI - SPHERICAL COMPONENTS OF THE MAIN
% GEOMAGNETIC FIELD IN NANOTESLA
% (POSITIVE BR OUTWARD, BTHETA SOUTHWARD, BPHI EASTWARD)
  GEOPACK_RECALC(r_time(1),iday,r_time(4),r_time(5),r_time(6));
  [br,bt,bf]=GEOPACK_IGRF_GEO(r,theta,phi);
  B=[-bt -bf br]*1e-9; %[N W U] Tesla
 end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %Eg=-cross(Vg(i,:),B); % V/m geographic
 [D,I,r]=cart2sph(B(1),B(2),B(3));
 gtm=[-sin(I)*cos(D) -sin(I)*sin(D) cos(I);-sin(D) cos(D) 0;-cos(I)*cos(D) -cos(I)*sin(D) -sin(I)]';
 Vm=Vg(i,:)*gtm; %geomagnetic
 %Em=Eg*gtm, % geomagnetic
 BB=[0 0 -norm(B)];
 Em=-cross(Vm,BB);
 E=[E;Em(1:2)]; %Along B = 0 always
 Vgvv=Vgv([1 4 6;4 2 5;6 5 3]);
 Vmv=Vgvv*abs(gtm);
 EE=abs(cross(sqrt(diag(Vmv)),BB));
 Ee=[Ee;EE(1:2)];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end
if add
 save(vfile,'Vdate','Vpos','Vg','Vgv','E','Ee')
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% plot data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% load Efield_CP2_2005-09-12_tau2pl_0@uhf.mat
START_TIME=floor(datevec(Vdate(1)));
END_TIME=ceil(datevec(Vdate(end)));
START_TIME=minput('Start time',START_TIME);
END_TIME=minput('  End time',END_TIME);

xlim=[datenum(START_TIME) datenum(END_TIME)];
fa=find(max(abs(E),[],2) < 0.1);
tickform='HH:MM';
d={'north' 'west'};
for i=1:2
 subplot(2,1,i)
 plot(mean(Vdate(:,fa)),E(fa,i)*1000,'-ok')
 hold on
 plot(ones(2,1)*mean(Vdate(:,fa)),1000*(ones(2,1)*E(fa,i)'+[1;-1]*Ee(fa,i)'),'-k')
 hold off
 set(gca,'XLim',xlim,'ylim',[-100 100])
 title(datestr(mean(mean(Vdate)))),xlabel('UT'),ylabel(['E ' d{i} ' [mV/m]'])
 set(gca,'xgrid','on','ygrid','on')
 datetick(gca,'x',tickform,'keeplimits')
end
