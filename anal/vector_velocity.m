% function [Vdate,Vpos,Vg,Vgv]=vector_velocity(dirs,alt,td,ld,uperr,mind,odir,dynavel)
% function [result_file]=vector_velocity(dirs,alt,td,ld,uperr,mind,odir)
% To calculate 3D velocities from EISCAT data
% Copyright EISCAT 2008-02-28
% Inputs:dirs	{result_path} Directories containing data (wild cards accepted)
%			Sites separated in cell
%	alt	[170 500] Altitude ranges to handle
%	td	[300 300 t0] Maximum time span, time step, first time
%			td<=number of dirs: that dir selects times
%	ld	[] Latitude ranges to handle (imaginary for invlat (slow))
%	uperr	[Inf 160] Constraint the vertical [from 160km parallel] comp
%	mind	[3 10] Minimum no of directions and angle difference
%	odir	[dirs|path_tmp] Output directory
%       dynavel [0|1|2|3] Use Tromso dynasonde vectors, bitpattern for F,E values
% Outputs:Vdate	(2,:) Datenum span for the estimate
%	Vpos	(:,3) Mean lat,lon,alt
%	Vg	(:,3) Geographic velocity (E,N,U)
%	Vgv	(:,6) Geographic velocity variance matrix, diagonals 0,1 and 2
%	: or result file name
% See also EFIELD VEL_WRAPPER
%
function [varargout]=vector_velocity(dirs,alt,td,ld,uperr,mind,odir,dynavel)
global result_path path_tmp path_GUP GUP_ver local
if nargin<1, dirs=[]; end
if nargin<2, alt=[]; end
if nargin<3, td=[]; end
if nargin<4, ld=[]; end
if nargin<5, uperr=[]; end
if nargin<6, mind=[]; end
if nargin<7, odir=[]; end
if nargin<8, dynavel=[]; end
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
 if ndir>1 || ~isempty(strfind(dirs{1},'*'))
  odir=path_tmp;
 else
  odir=dirs{1};
 end
 odir=fullfile(odir,filesep);
end
if isempty(dynavel), dynavel=0; end
%dirs='/home/ingemar/tmp/2007-02-07_tau2pl_ant@uhf/';
%%%%%%%%%%%%%%%%%
global r_RECloc allnames r_XMITloc
Data1D=[]; Data2D=[]; dirind=0; loc=[]; allnames=[];
for d1=1:ndir
 if dirs{d1}=='.'; dirs{d1}=pwd; end
 fprintf('Reading %s...\n',dirs{d1})
 [Time,par2D,par1D,dum,err2D]=load_param(dirs{d1},[1,Inf]);
 ng=size(par2D,1);
 [g,dump]=find(par2D(:,:,2)>alt(1) & par2D(:,:,2)<alt(end) & isfinite(par2D(:,:,6)));
 [d,dum,dd]=unique(dump); dd=dd+size(Data1D,1);
 Data1D=[Data1D;[Time(:,d)' par1D(d,1:2)]]; % only time+el+az used
 D2D=[reshape(par2D(:,:,[1 2 6]),[],3) col(err2D(:,:,4))]; % only alt+ran+vi+vie used
 Data2D=[Data2D;[D2D(g+(dump-1)*ng,:) dd(:)]];
 dirind=[dirind;length(dump)];
 loc=[loc;r_RECloc r_XMITloc];
 if td(1)==d1
  timint=mean(Time)-median(diff(Time));
 end
end
dirind=cumsum(dirind);
r_time=datevec(mean(Data1D(:,1)));
%%%%%%%%%%%%%%%%%
if dynavel
 t1=min(Data1D(:,1)); t2=max(Data1D(:,1));
 tsound=225/86400;
 if dynavel>1
  [dydE,dyvE]=get_v(t1,t2,'tromso','E');
  if min(diff(dydE))<tsound, tsound=min(diff(dydE)), end
 end
 if rem(dynavel,2)
  [dydF,dyvF]=get_v(t1,t2,'tromso','F');
  if min(diff(dydF))<tsound, tsound=min(diff(dydF)), end
 end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if isfinite(ld(1))
 ILAT=0;
 if ~isreal(ld) || max(ld)>90
  ld=abs(ld);
  ILAT=1;
 end
 if libisloaded('onera_desp_lib')
  ILAT=1;
 else
  addpath(fullfile(path_GUP,'models_m'),'-begin')
  [secs,YEAR]=tosecs(r_time);
  magF=IGRF(); DIMO=magF.FELDCOF(YEAR+secs/86400/365);
 end
end
degrad=pi/180.;  % conversion factor from degrees to radians
Re=6378.135;
min_area=sqrt(3)/4*(mind(2)*degrad)^2; % minimum equilateral triange angle area to cover
tfile=0;	%make velocity table for testings
%%%%%%%%%%%%%%%%%
dates=mean(Data1D(:,[1 2]),2); td=td/86400; ld=ld*degrad;
mindumps=mind(1)-isfinite(uperr(1));
loc0=reshape([gg2gc(loc(:,1:3)) gg2gc(loc(:,4:6))]',3,2,[]);
loc0=reshape(mean(loc0,2),3,[])'; %Effective position for bistatic
if td(1)>ndir/86400
 if length(td)==2, td(3)=floor(min(Data1D(:,1))); end
 timint=td(3):td(2):max(Data1D(:,2))+td(2);
else
 td(1)=median(diff(timint));
end
%%%%%%%%%%%%%%%%%
name_ants=allnames.ant; nant=size(allnames.ant,1);
name_exps=allnames.expr;
name_sigs=[];
if isfield(allnames,'sig'), name_sigs=allnames.sig; end
name_strategies=[];
if isfield(allnames,'strategy'), name_strategies=allnames.strategy; end

if nant==1
 name_ant=allnames.ant; name_ants=[];
elseif all(contains(cellstr(allnames.ant(:,1:3)),{'32m' '42m'}))
 name_ant='esr';
elseif all(contains(cellstr(allnames.ant(:,1:3)),{'uhf' 'vhf'}))
 name_ant='tro';
elseif all(contains(cellstr(allnames.ant(:,1:3)),{'uhf' 'vhf' 'kir' 'sod'}))
 name_ant='kst';
else
 name_ant='esa';
end
if size(allnames.expr,1)==1
 nexp=['_' name_exps]; name_expr=name_exps; name_exps=[];
else
 nexp=[]; name_expr=[];
end
name_strategy='Altitude';
if isfinite(ld(1)), name_strategy=[name_strategy ' Latitude']; end
if isfinite(uperr(1)), name_strategy=[name_strategy ' Model']; end
if dynavel, name_strategy=[name_strategy ' Dynasond']; end
name_strategy=[name_strategy sprintf(' %d',round(86400*td(1)))];
nstrat=regexp(name_strategy,'[A-Z0-9]');
oname=sprintf('%d-%02d-%02d%s_V%d%s@%s',r_time(1:3),nexp,nant,lower(name_strategy(nstrat)),name_ant);
result_file=fullfile(odir,oname);
if tfile
 tfile=fopen([result_file '.txt'],'w');
 fprintf(tfile,'%20s%6s%6s%4s','Date     Time','Lat','Lon','Alt');
 fprintf(tfile,'%5s','VgE','VgN','VgU','VgEe','VgNe','VgUe');
 fprintf(tfile,'\n');
 fprintf(tfile,'%20s%6s%6s%4s','UT','deg','deg','km');
 fprintf(tfile,'%5s',[],'m/s',[],[],'m/s');
 fprintf(tfile,'\n');
end
Vdate=[]; Vpos=[]; Vg=[]; Vgv=[]; V_area=[]; Gid=[];
%%%%%%%%%%%%%%%%%
fprintf('Combining...\n')
for tim=timint
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
    [dum,site]=histc(g,dirind+.5);
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
     else
      L=zeros(ng,1);
      for i=1:ng
       L(i)=magF.SHELLG(gg(i,1),gg(i,2),gg(i,3),DIMO);
      end
      %B=geomag(gg',r_time);
      %d=-asin(B(3,:)./sqrt(sum(B(1:3,:).^2)));
      %mlat=real(asin(d./sqrt(d.^2+cos(gg(:,1)'*degrad)))); %modip
     end
     mlat=acos(sqrt(1../abs(L))); %inv lat
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for il=1:length(ld)-1
     if isfinite(ld(1))
      ll=find(mlat>ld(il) & mlat<ld(il+1));
     else
      ll=1:ng;
     end
     gid=sum(log(g(ll))); %ID for the selection
     seldumps=unique(dump(ll));
     if length(seldumps)>=mindumps & ~any(gid==Gid)
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
        up=15.*cos((rem(datenum(r_time),1)-21.5/24.)*2*pi);
        Vll=[Vll;up];
        Vlle=[Vlle;uperr(1)];
       else
        A=[A;[0 0 1]];
        Vll=[Vll;0];
        Vlle=[Vlle;uperr(1)];
       end
      end
      if dynavel & (~isfinite(ld(il)) | ld(il)<69.6 & ld(il+1)>69.6)
       d=[];
       if alti>=uperr(2) & rem(dynavel,2)
	d=find(dydF+tsound/2>tim & dydF+tsound/2<tim+td(1));
        dyv=dyvF(d,:);
       elseif alti<uperr(2) & dynavel>1
	d=find(dydE+tsound/2>tim & dydE+tsound/2<tim+td(1));
        dyv=dyvE(d,:);
       end
       ldy=length(d);
       if ldy
        err=sqrt(sum(dyv(:,1:3).^2,2).*dyv(:,4))/100;
        for comp=1:3
	 oo=zeros(ldy,3); oo(:,comp)=1;
         A=[A;oo];
         Vll=[Vll;dyv(:,comp)];
         Vlle=[Vlle;err];
	end
       end
      end
      ang_area=angarea(A);
      if ang_area>min_area
%%A*V_real=Vll +- Vlle
%%V_real=A\Vll;
%%[V_real,Verr_real]=lscov(A,Vll,1../Vlle.^2);
       VV=(Vll./Vlle.^2)'*A;
       T=(A'*(A./(Vlle.^2*ones(1,3))));
       V_real=VV/T;
       if isposdef(T)
        T=T^-1;
        Vvar_real=T([1 5 9 2 6 3]); %lower triangle only
        Verr=sqrt(Vvar_real(1:3));
       else
	warning('Covariance matrix not positive definite (%s)',datestr(mean(Date),13))
	% trying direct route, but errors handled in a clumsy way
        [V_real,Verr]=lscov(A,Vll,1../Vlle.^2);
        if size(A,1)==3
         Verr=abs(A)\Vlle;
        end
        Vvar_real=[Verr'.^2 zeros(1,3)];
        V_real=V_real';
       end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       if all(real(Verr)>=0)
        selDate=[min(Data1D(seldumps,1));max(Data1D(seldumps,2))];
        if tfile
         fprintf(tfile,'%s',datestr(mean(selDate)));
         fprintf(tfile,'%6.2f',gg_sp(1:2));
         fprintf(tfile,'%4.0f',gg_sp(3));
         fprintf(tfile,'%5.0f',V_real);
         fprintf(tfile,'%5.0f',Verr);
         fprintf(tfile,'\n');
        end
        Vdate=[Vdate selDate];
        Vpos=[Vpos;gg_sp];
        Vg=[Vg;V_real];
        Vgv=[Vgv;Vvar_real];
        V_area=[V_area;ang_area];
        Gid=[Gid gid];
       else
	warning('Shortcuts did not help, --skipping')
       end
      end
     end
    end
   end
  end
 end
end
if nargout>1
 varargout={Vdate,Vpos,Vg,Vgv};
else
 if nargout==1
  varargout={result_file};
 else
  result_file
 end 
 Vinputs=struct('InputData',dirs,'AltitudeRange',alt,'TimeSpan',td,'LatitudeRange',ld,'UpConstraint',uperr,'MinDir',mind,'DynasondeVelocity',dynavel);
 name_sig=[local.host ' ' local.user ' ' datestr(now)];
 save_noglobal([result_file '.mat'],Vdate,Vpos,Vg,Vgv,V_area,name_exps,name_expr,name_ant,name_ants,name_sig,name_sigs,name_strategy,name_strategies,GUP_ver,Vinputs)

 fprintf('Making NCAR file...\n')
 NCAR_output
 NCAR_output(result_file,[],fullfile(odir,['NCARv_' oname '.bin']))
 NCAR_output

 matvecvel2hdf5([result_file '.mat'],odir);
end
if tfile, fclose(tfile); end
return

function area=angarea(enu)
% calculate the "angle area" for a number of directions
[t,p]=cart2sph(enu(:,1),enu(:,2),enu(:,3));
[x,y]=pol2cart(t,pi/2-p);
try
 k=convhull(x,y);
catch
 try
  global local
  if local.matlabversion<8
   k=convhull(x,y,{'QJ'});
  else
   k=convhull(x,y,'simplify',true);
  end
 catch
  [t,p]=cart2pol(x-median(x),y-median(y));
  [dum,k]=sort(t);
  warning('Doing rough area calc')
 end
end
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

function ret=isposdef(M)
ret=true;
for i=1:length(M)
  if det(M(1:i,1:i))<=0
    ret=false;
    break
  end
end
return
