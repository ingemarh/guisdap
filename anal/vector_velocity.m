function [varargout]=vector_velocity(dirs,alt,td,parerr,verterr,odir)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
global result_path path_tmp path_GUP
if nargin<1, dirs=[]; end
if nargin<2, alt=[]; end
if nargin<3, td=[]; end
if nargin<4, parerr=[]; end
if nargin<5, verterr=[]; end
if nargin<6, odir=[]; end
if isempty(dirs), dirs=result_path; end
if isempty(alt), alt=[170 500]; end
if isempty(td), td=300; end
dirs=fullfile(dirs,filesep);
if isempty(odir)
 if strfind(dirs,'*')
  odir=path_tmp;
 else
  odir=dirs;
 end
end
%dirs='/home/ingemar/tmp/2007-02-07_tau2pl_ant@uhf/';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mexing=1;
if ~isempty(parerr) & ~exist('geomag')
 mexing=0;
 path(path,fullfile(path_GUP,'models','geopack'))
end
degrad=pi/180.;  % conversion factor from degrees to radians
r_earth=6378.135; % earth radius (km) and flatness factor
[Time,par2D,par1D,dum,err2D]=load_param(dirs);
global r_RECloc name_ant name_expr
%%%%%%%%%%%%%%%%%
r_time=datevec(Time(2,1));
fig=sprintf('Vecvel_%d-%02d-%02d',r_time(1:3));
if exist(fullfile(dirs,'.gup'),'file')
 load('-mat',fullfile(dirs,'.gup'))
 fig=sprintf('%s_%s_%d@%s',fig,name_expr,intper,name_ant);
end
result_file=fullfile(odir,fig),
fid2=fopen([result_file '.txt'],'w');
fprintf(fid2,'%20s%6s%6s%4s','Date     Time','Lat','Lon','Alt');
fprintf(fid2,'%5s','VgN','VgW','VgU','VgNe','VgWe','VgUe');
fprintf(fid2,'\n');
fprintf(fid2,'%20s%6s%6s%4s','UT','deg','deg','km');
fprintf(fid2,'%5s',[],'m/s',[],[],'m/s');
fprintf(fid2,'\n');
%%%%%%%%%%%%%%%%%
[aa,count]=min(abs(par2D(:,:,2)-alt(1)));
count=count+(0:size(par2D,2)-1)*size(par2D,1);
ng=size(par2D,1); l2D=size(par2D,1)*size(par2D,2);
%%%%%%%%%%%%%%%%%
Vdate=[]; Vpos=[]; Vg=[]; Vgv=[];
%%%%%%%%%%%%%%%%%
d=find(par1D(:,2) < 74 | par1D(:,2) > 69)';  % remove Tro FA data
t1=floor(Time(1,1));
for tim=t1:td/86400:Time(2,end)
 count=d(find(Time(2,d)>=tim & Time(1,d)<=tim+td/86400));
 if length(count)>=3
  [g,dump]=find(par2D(:,count,2) > alt(1) & par2D(:,count,2) < alt(2) & isfinite(par2D(:,count,6)));
  dump=count(dump)';
  if length(unique(dump))>=3
   g=g+(dump-1)*ng;
   az=par1D(dump,1);el=par1D(dump,2);
   Vi=par2D(g+(6-1)*l2D);
   Vierr=err2D(g+(4-1)*l2D);
   r_range=par2D(g);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   Date=[Time(1,dump(1));Time(2,dump(end))];
   r_time=datevec(mean(Date));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   gc=zeros(1,3);
   for i=1:length(az)
    gg_sp=loc2gg(r_RECloc,[el(i) az(i) r_range(i)]);
    gc=gc+gg2gc(gg_sp);
   end
   gg_sp=gc2gg(gc/length(az));
   obs_lat=gg_sp(1);
   obs_lon=gg_sp(2);
   alt_0=gg_sp(3);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   az1=-az*degrad; % -Az
   el1=(90-el)*degrad; % 90-El
   A=[sin(el1).*cos(az1) sin(el1).*sin(az1) cos(el1)];
   if isfinite(parerr)
    if mexing
     bpar=geomag(gg_sp,r_time);
     B=[bpar(1) -bpar(2) -bpar(3)]; %[N W U] Tesla
    else
     iday=(r_time(2)-1)*30+r_time(3);
     r=alt_0/r_earth+1;
     theta=(90.-obs_lat)*degrad;
     phi=obs_lon*degrad;
% BR, BTHETA, BPHI - SPHERICAL COMPONENTS OF THE MAIN
% GEOMAGNETIC FIELD IN NANOTESLA
% (POSITIVE BR OUTWARD, BTHETA SOUTHWARD, BPHI EASTWARD)
     GEOPACK_RECALC(r_time(1),iday,r_time(4),r_time(5),r_time(6));
     [br,bt,bf]=GEOPACK_IGRF_GEO(r,theta,phi);
     B=[-bt -bf br]*1e-9; %[N W U] Tesla
    end
    A=[A;-B/norm(B)];
    Vi=[Vi;0];
    Vierr=[Vierr;parerr];
   elseif isfinite(verterr)
    A=[A;[0 0 1]];
    Vi=[Vi;0];
    Vierr=[Vierr;verterr];
   end
% A x V_real=V_obs
% ->     V_real = A* x A x V_real = A* x V_obs = A\V_obs
% example   Vi_t=100;Vi_k=50;Vi_s=30;
%                Vi_t=100;Vi_k=0;Vi_s=0;
%%%V_real=A\Vi_0;
%V_real(1);% Northward
%V_real(2);% Westward
%V_real(3);% Upward
%%%[V_real,Verr_real,MSE,S]=lscov(A,Vi_0,1../Vierr_0.^2),
   VV=(Vi./Vierr.^2)'*A;
   T=(A'*(A./(Vierr.^2*ones(1,3))));
   V_real=VV/T;
   T=T^-1;
   Vvar_real=T([1 5 9 2 6 3]); %lower triangle only
   Verr=sqrt(Vvar_real([1:3]));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   fprintf(fid2,'%s',datestr(mean(Date)));
   fprintf(fid2,'%6.2f',obs_lat);
   fprintf(fid2,'%6.2f',obs_lon);
   fprintf(fid2,'%4.0f',alt_0);
   fprintf(fid2,'%5.0f',V_real);
   fprintf(fid2,'%5.0f',Verr);
   fprintf(fid2,'\n');
   Vdate=[Vdate Date];
   Vpos=[Vpos;[obs_lat obs_lon alt_0]];
   Vg=[Vg;V_real];
   Vgv=[Vgv;Vvar_real];
  end
 end
end
save(result_file,'Vdate','Vpos','Vg','Vgv')
if nargout>0
 varargout={Vdate,Vpos,Vg,Vgv};
end
