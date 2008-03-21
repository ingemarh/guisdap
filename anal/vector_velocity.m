% function [Vdate,Vpos,Vg,Vgv]=vector_velocity(dirs,alt,td,ld,uperr,mind,odir)
% To calculate 3D velocities from EISCAT data
% Copyright EISCAT 2008-02-28
% Inputs:dirs	{result_path} Directories containing data (wild cards accepted)
%			Sites separated in cell
%	alt	[170 500] Altitude ranges to handle
%	td	[300 300 t0] Maximum time span, time step, first time
%	ld	[] Latitude ranges to handle (imaginary for invlat (slow))
%	uperr	[Inf 160] Constraint the vertical [from 160km parallel] comp
%	mind	[3 10] Minimum no of directions and angle difference
%	odir	[dirs/path_tmp] Output directory
% Outputs:Vdate	(2,:) Datenum span for the estimate
%	Vpos	(:,3) Mean lat,lon,alt
%	Vg	(:,3) Geographic velocity (E,N,U)
%	Vgv	(:,6) Geographic velocity variance matrix, diagonals 0,1 and 2
% See also EFIELD
%
function [varargout]=vector_velocity(dirs,alt,td,ld,uperr,mind,odir)
global result_path path_tmp path_GUP
if nargin<1, dirs=[]; end
if nargin<2, alt=[]; end
if nargin<3, td=[]; end
if nargin<4, ld=[]; end
if nargin<5, uperr=[]; end
if nargin<6, mind=[]; end
if nargin<7, odir=[]; end
if isempty(dirs), dirs={result_path}; end
if ~iscell(dirs), dirs={dirs}; end
if isempty(alt), alt=[170 500]; end
if isempty(td), td=300; end
if length(td)==1, td(2)=td(1); end
if isempty(ld), ld=[NaN NaN]; end
if isempty(uperr), uperr=Inf; end
if length(uperr)==1
 uperr(2)=160; % Switchover altitude for geomagnetic orientation of forcing
end
if isempty(mind), mind=3; end
if length(mind)==1, mind(2)=10; end
ndir=length(dirs);
if isempty(odir)
 if ndir>1 | strfind(char(dirs(1)),'*')
  odir=path_tmp;
 else
  odir=char(dirs(1));
 end
 odir=fullfile(odir,filesep);
end
%dirs='/home/ingemar/tmp/2007-02-07_tau2pl_ant@uhf/';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if exist('geomag')~=3
 addpath(fullfile(path_GUP,'models','geopack'),'-end')
end
if isfinite(ld(1))
 ILAT=0;
 if max(ld)>90
  addpath(fullfile(path_GUP,'models','onera','matlab'),'-end')
  try
   onera_desp_lib_load
   ILAT=1;
  catch
  end
 end
end
degrad=pi/180.;  % conversion factor from degrees to radians
Re=6378.135;
min_area=sqrt(3)/4*(mind(2)*degrad)^2; % minimum equilateral triange angle area to cover
%%%%%%%%%%%%%%%%%
global r_RECloc name_ant name_expr r_XMITloc
Data1D=[]; Data2D=[]; dirind=0; loc=[];
for d1=dirs
 [Time,par2D,par1D,dum,err2D]=load_param(char(d1));
 ng=size(par2D,1);
 [g,dump]=find(par2D(:,:,2)>alt(1) & par2D(:,:,2)<alt(end) & isfinite(par2D(:,:,6)));
 [d,dum,dd]=unique(dump);
 Data1D=[Data1D;Time(:,d)' par1D(d,1:2)]; % only time+el+az used
 D2D=[reshape(par2D(:,:,[1 2 6]),[],3) col(err2D(:,:,4))]; % only alt+ran+vi+vie used
 Data2D=[Data2D;D2D(g+(dump-1)*ng,:) dd];
 dirind=[dirind;length(dump)];
 loc=[loc;r_RECloc r_XMITloc];
end
dirind=cumsum(dirind);
%%%%%%%%%%%%%%%%%
r_time=datevec(mean(Data1D(:,1)));
fig=sprintf('Vecvel_%d-%02d-%02d',r_time(1:3));
if ndir==1
 fig=sprintf('%s_%s@%s',fig,name_expr,name_ant);
end
result_file=fullfile(odir,fig),
fid=fopen([result_file '.txt'],'w');
fprintf(fid,'%20s%6s%6s%4s','Date     Time','Lat','Lon','Alt');
fprintf(fid,'%5s','VgE','VgN','VgU','VgEe','VgNe','VgUe');
fprintf(fid,'\n');
fprintf(fid,'%20s%6s%6s%4s','UT','deg','deg','km');
fprintf(fid,'%5s',[],'m/s',[],[],'m/s');
fprintf(fid,'\n');
Vdate=[]; Vpos=[]; Vg=[]; Vgv=[];
%%%%%%%%%%%%%%%%%
dates=mean(Data1D(:,[1 2]),2); td=td/86400; ld=ld*degrad;
if length(td)==2, td(3)=floor(min(Data1D(:,1))); end
mindumps=mind(1)-isfinite(uperr(1));
loc0=reshape([gg2gc(loc(:,1:3)) gg2gc(loc(:,4:6))]',3,2,[]);
loc0=reshape(mean(loc0,2),3,[])'; %Effective position for bistatic
%%%%%%%%%%%%%%%%%
for tim=td(3):td(2):max(Data1D(:,2))+td(2)
 count=find(dates>=tim & dates<=tim+td(1));
 if length(count)>=mindumps
  gates=find(ismember(Data2D(:,end),count));
  for ia=1:length(alt)-1
   g=gates(find(Data2D(gates,2)>alt(ia) & Data2D(gates,2)<alt(ia+1)));
   dump=Data2D(g,end); d=unique(dump);
   if length(d)>=mindumps
    Date=[min(Data1D(d,1));max(Data1D(d,2))];
    r_time=datevec(mean(Date));
    alti=mean(alt(ia:ia+1));
    [dum,site]=histc(dump,dirind+.5);
    Vi=Data2D(g,3);
    Vierr=Data2D(g,4);
    loc_sp=[Data1D(dump,[4 3]) Data2D(g,1)]; 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ng=length(g); gg=zeros(ng,3);
    for i=1:ng
     gg(i,:)=loc2gg(loc(site(i),1:3),loc_sp(i,:));
    end
    gc=gg2gc(gg);
    if isfinite(ld(1))
     if ILAT
      gcR=gc/Re;
      L=onera_desp_lib_make_lstar([],[],'geo',mean(Date),gcR(:,1),gcR(:,2),gcR(:,3));
      mlat=acos(sqrt(1../abs(L))); %inv lat
     else
      B=geomag(gg',r_time);
      d=-asin(B(3,:)./sqrt(sum(B(1:3,:).^2)));
      mlat=real(asin(d./sqrt(d.^2+cos(gg(:,1)'*degrad)))); %modip
     end
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for il=1:length(ld)-1
     if isfinite(ld(1))
      ll=find(mlat>ld(il) & mlat<ld(il+1));
     else
      ll=1:ng;
     end
     if length(unique(dump(ll)))>=mindumps
      gg_sp=gc2gg(mean(gc(ll,:)));
      %az1=az(ll)*degrad; el1=el(ll)*degrad;
      %A=[cos(el1).*sin(az1) cos(el1).*cos(az1) sin(el1)];
      [loce,locn,locu]=xyz2enu(gc(ll,1),gc(ll,2),gc(ll,3),loc0(site(ll),1),loc0(site(ll),2),loc0(site(ll),3)); 
      A=-[loce locn locu]./(sqrt(loce.^2+locn.^2+locu.^2)*ones(1,3));
      Vll=Vi(ll); Vlle=Vierr(ll);
      if isfinite(uperr(1))
       if alti>uperr(2)
        B=geomag(gg_sp',r_time)';
        A=[A;-B(1:3)/norm(B(1:3))];
        Vll=[Vll;0];
        Vlle=[Vlle;uperr(1)];
       else
        A=[A;[0 0 1]];
        Vll=[Vll;0];
        Vlle=[Vlle;uperr(1)];
       end
      end
      if angarea(A)>min_area
%%A*V_real=Vll +- Vlle
%%V_real=A\Vi_0;
%%[V_real,Verr_real]=lscov(A,Vll,1../Vlle.^2);
       VV=(Vll./Vlle.^2)'*A;
       T=(A'*(A./(Vlle.^2*ones(1,3))));
       V_real=VV/T;
       T=T^-1;
       Vvar_real=T([1 5 9 2 6 3]); %lower triangle only
       Verr=sqrt(Vvar_real([1:3]));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       fprintf(fid,'%s',datestr(mean(Date)));
       fprintf(fid,'%6.2f',gg_sp(1:2));
       fprintf(fid,'%4.0f',gg_sp(3));
       fprintf(fid,'%5.0f',V_real);
       fprintf(fid,'%5.0f',Verr);
       fprintf(fid,'\n');
       Vdate=[Vdate Date];
       Vpos=[Vpos;gg_sp];
       Vg=[Vg;V_real];
       Vgv=[Vgv;Vvar_real];
      end
     end
    end
   end
  end
 end
end
save(result_file,'Vdate','Vpos','Vg','Vgv')
if nargout>0
 varargout={Vdate,Vpos,Vg,Vgv};
end
return

function area=angarea(enu)
% calculate the "angle area" for a number of directions
[t,p]=cart2sph(enu(:,1),enu(:,2),enu(:,3));
[x,y]=pol2cart(t,pi/2-p);
k=convhull(x,y);
area=polyarea(x(k),y(k));
return

function [e,n,u]=xyz2enu(Xr,Yr,Zr,X,Y,Z) 
% convert ECEF coordinates to local east, north, up 
% taken from http://en.wikipedia.org/wiki/Geodetic_system
phiP=atan2(Zr,sqrt(Xr.^2 + Yr.^2)); 
lambda=atan2(Yr,Xr); 
e=-sin(lambda).*(X-Xr)+cos(lambda).*(Y-Yr); 
n=-sin(phiP).*cos(lambda).*(X-Xr)-sin(phiP).*sin(lambda).*(Y-Yr)+cos(phiP).*(Z-Zr); 
u=cos(phiP).*cos(lambda).*(X-Xr)+cos(phiP).*sin(lambda).*(Y-Yr)+sin(phiP).*(Z-Zr);
return
