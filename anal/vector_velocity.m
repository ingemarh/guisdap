% function [Vdate,Vpos,Vg,Vgv]=vector_velocity(dirs,alt,td,uperr,ld,odir)
% To calculate 3D velocities from EISCAT data
% Copyright EISCAT 2008-02-28
% Inputs:dirs	[result_path] Directory containing data (wild cards accepted)
%	alt	[170 500] Altitude ranges to handle
%	td	[300 300] Maximum time span, time step
%	uperr	[Inf Inf] Apply constraints on the [parallel vertical] component
%	ld	[] Dip latitude ranges to handle
%	odir	[dirs/path_tmp] Output directory
% Outputs:Vdate	(2,:) Datenum span for the estimate
%	Vpos	(:,3) Mean lat,lon,alt
%	Vg	(:,3) Geographic velocity (E,N,U)
%	Vgv	(:,6) Geographic velocity variance matrix, diagonals 0,1 and 2
% See also EFIELD
%
function [varargout]=vector_velocity(dirs,alt,td,uperr,ld,odir)
global result_path path_tmp path_GUP
if nargin<1, dirs=[]; end
if nargin<2, alt=[]; end
if nargin<3, td=[]; end
if nargin<4, uperr=[]; end
if nargin<5, ld=[]; end
if nargin<6, odir=[]; end
if isempty(dirs), dirs=result_path; end
if isempty(alt), alt=[170 500]; end
if isempty(td), td=300; end
if length(td)==1, td(2)=td(1); end
if isempty(uperr), uperr=Inf; end
if length(uperr)==1, uperr(2)=Inf; end
if isempty(ld), ld=[NaN NaN]; end
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
if exist('geomag')~=3
 addpath(fullfile(path_GUP,'models','geopack'),'-end')
end
degrad=pi/180.;  % conversion factor from degrees to radians
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
fid=fopen([result_file '.txt'],'w');
fprintf(fid,'%20s%6s%6s%4s','Date     Time','Lat','Lon','Alt');
fprintf(fid,'%5s','VgE','VgN','VgU','VgEe','VgNe','VgUe');
fprintf(fid,'\n');
fprintf(fid,'%20s%6s%6s%4s','UT','deg','deg','km');
fprintf(fid,'%5s',[],'m/s',[],[],'m/s');
fprintf(fid,'\n');
%%%%%%%%%%%%%%%%%
ng=size(par2D,1); l2D=size(par2D,1)*size(par2D,2);
dates=mean(Time); td=td/86400; ld=ld*degrad;
%%%%%%%%%%%%%%%%%
Vdate=[]; Vpos=[]; Vg=[]; Vgv=[];
%%%%%%%%%%%%%%%%%
t1=floor(Time(1,1)); mindump=3;
for tim=t1:td(2):Time(2,end)
 count=find(dates>=tim & dates<=tim+td(1));
 if length(count)>=3
  for ia=1:length(alt)-1
   [g,dump]=find(par2D(:,count,2)>alt(ia) & par2D(:,count,2)<alt(ia+1) & isfinite(par2D(:,count,6)));
   dump=count(dump)';
   if length(unique(dump))>=mindump
    g=g+(dump-1)*ng;
    az=par1D(dump,1);el=par1D(dump,2);
    Vi=par2D(g+(6-1)*l2D);
    Vierr=err2D(g+(4-1)*l2D);
    r_range=par2D(g);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    Date=[Time(1,dump(1));Time(2,dump(end))];
    r_time=datevec(mean(Date));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ntotest=length(az);
    gc=zeros(ntotest,3); gg_sp=gc;
    for i=1:ntotest
     gg_sp(i,:)=loc2gg(r_RECloc,[el(i) az(i) r_range(i)]);
     gc(i,:)=gg2gc(gg_sp(i,:));
    end
    if isfinite(ld(1))
     B=geomag(gg_sp',datevec(mean(Date)));
     d=-asin(B(3,:)./sqrt(sum(B(1:3,:).^2)));
     modip=real(asin(d./sqrt(d.^2+cos(gg_sp(:,1)'*degrad))));
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for il=1:length(ld)-1
     if isfinite(ld(1))
      ll=find(modip>ld(il) & modip<ld(il+1));
     else
      ll=1:ntotest;
     end
     if length(unique(dump(ll)))>=mindump
      gg_sp=gc2gg(mean(gc(ll,:)));
      az1=az(ll)*degrad;
      el1=el(ll)*degrad;
      A=[cos(el1).*sin(az1) cos(el1).*cos(az1) sin(el1)];
      Vll=Vi(ll); Vlle=Vierr(ll);
      if isfinite(uperr(1))
       B=geomag(gg_sp',r_time)';
       A=[A;-B(1:3)/norm(B(1:3))];
       Vll=[Vll;0];
       Vlle=[Vlle;uperr(1)];
      end
      if isfinite(uperr(2))
       A=[A;[0 0 1]];
       Vll=[Vll;0];
       Vlle=[Vlle;uperr(2)];
      end
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
save(result_file,'Vdate','Vpos','Vg','Vgv')
if nargout>0
 varargout={Vdate,Vpos,Vg,Vgv};
end
