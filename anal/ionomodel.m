% ionomodel.m: user-supplied ionospheric model
% GUISDAP v.1.80 01-07-27 Copyright EISCAT, Huuskonen&Lehtinen
% 
% 'ionomodel' is a user-supplied function which for a given set of heights
% outputs the plasma parameters (Ne, Ti, Te/Ti, coll, [O+]/Ne, velocity)
% with their a priori uncertainties. This simple example is static, a more complete
% model might use the date and time values to modify the model.
% ionomodel_control==6 comp=f(sun)
%                    7 comp=f(rawne)
%                    8 comp=f(modelne)
%
% function [apriori,apriorierror]=ionomodel(heights);

function [apriori,apriorierror]=ionomodel(heights,ne_from_pp)

global ionomodel_control iono_model p_m0 fit_altitude name_site ionomodel_first

len=length(heights);
heights=col(heights);
modinfo=0;

if isempty(ionomodel_control), ionomodel_control=0; end
if isempty(iono_model), iono_model='giveme'; end
if isempty(ionomodel_first)
 ionomodel_first=0; modinfo=1;
end
nion=length(p_m0);
if any([6+nion 5]-size(fit_altitude)>0)
 ff=fit_altitude;
 % Altitudes (h2>h1) where to fit each parameter resp
 % Strucure: h1 h2 hd(transition height) maxerr minerr relerr(relative or static errors)
 fit_altitude=[0  Inf 0 1e2 1
              80  Inf 0 1e4 0
             107 1500 0 1e1 0
              90  107 0 1e2 1
               0  Inf 0 1e5 0
       repmat([0    0 0   1 0],nion-1,1)
               0    0 0 1e5 0
               0    0 0 9e9 0];
 if name_site=='L'
  fit_altitude(2:4,1:2)=[90 Inf;113 1500;0 0];
 elseif name_site=='V'
  fit_altitude(2:4,1:2)=[100 Inf;120 1500;0 0];
 end
 fit_altitude(1:size(ff,1),1:size(ff,2))=ff;
end

model=['ionomodel_' iono_model];
if exist([model '.m'])
 [altitude,ne,te,ti,coll,cO,cM2,cH]=feval(model,heights,modinfo);
else
 error(['Undefined ionospheric model: ' iono_model ' (giveme,gup150,iri)']);
end
heights2=heights;
heights=max(heights,altitude(1));
heights=min(heights,altitude(end));


apriori=ones(len,6+nion);
% Electron density
apriori(:,1)=exp(inter3(heights2,altitude,log(ne)))';

% Ion temperature
apriori(:,2)=inter3(heights,altitude,ti)';
 
% Temperature ratio
apriori(:,3)=inter3(heights,altitude,te)'./apriori(:,2);
 
% Ion-neutral collision frequency
apriori(:,4)=exp(inter3(heights2,altitude,log(coll)))';
 
% Ion velocity
apriori(:,5)=zeros(len,1);

% Clutters
apriori(:,(1:2)+4+nion)=zeros(len,2);

% Ion composition
if nion==3 & sum(p_m0-[16 1 30.5])==0
  ionc=[cH cM2];
elseif nion==2 & sum(p_m0-[30.5 16])==0
  ionc=1-cM2;
elseif nion==2 & sum(p_m0-[16 1])==0
  ionc=cH;
elseif nion~=1
  error(['No model for this ion mixture!']);
end
if nion>1
 apriori(:,6)=inter3(heights',altitude,ionc(:,1))';
 apriori(find(apriori(:,6)<0),6)=0;
 if nion==3
  apriori(:,7)=inter3(heights',altitude,ionc(:,2))';
  apriori(find(apriori(:,7)<0),7)=0;
 end
end

if nargout>1
 if ionomodel_control>5 & nion==2 & ~any(p_m0-[30.5 16])
  global pp_height
  if ~isempty(pp_height) & ionomodel_control==7
   global pp_profile p_N0 di_figures
   nepp=pp_profile*p_N0; hpp=pp_height';
   forc=log([1e11;300;100;1e11;120;25]); %;1e11;170;25]);
   tol=log([ 10  ;1.5;2  ;10  ;1.3;1.5]);%;10  ;2  ;2]);
   fac=(hpp/210).^2*median(nepp);
%  nepp=ne_from_pp; hpp=heights; fac=median(nepp);
   opts=optimset(optimset('fminsearch'),'MaxFunEvals',10000);
   chap=fminsearch('fit_chaps',forc,opts,nepp,hpp,forc,tol,fac,1);
   chap=exp(reshape(chap,3,[]));
   Ne210=chapman(210,chap);
   nepp=chapman(heights2,chap);
   if di_figures(2)
    hold on,plot(ne_from_pp/1e11,heights2,'g',nepp/1e11,heights2,'b'),hold off,drawnow
%   max(fit_chaps(log(chap),nepp,hpp,forc,tol,fac,0))
%   hold on,plot(fit_chaps(log(chap),nepp,hpp,forc,tol,fac,0),[hpp',10:10:60],'go'),hold off,drawnow
   end
   ne_from_pp=nepp+1e9;
  elseif ionomodel_control<8
   global d_time p_XMITloc
   tsec=tosecs(d_time(1,:));
   Ne210=[tsec p_XMITloc(1:2)];
  else
   Ne210=exp(inter3(210,altitude,log(ne)))';
  end
  apriori(:,6)=comp_model(heights2,Ne210);
 end
 if ionomodel_control~=1
  d=find(ne_from_pp~=0); apriori(d,1)=ne_from_pp(d);
 end

 apriorierror=ones(size(apriori));
 for par=1:size(apriori,2)
  h1=fit_altitude(par,1); h2=fit_altitude(par,2); hd=fit_altitude(par,3)+eps;
  err=(tanh((heights-h1)/hd)+tanh((h2-heights)/hd))/2;
  apriorierror(:,par)=err*fit_altitude(par,4);
 end
 par=find(fit_altitude(:,5));
 apriorierror(:,par)=apriorierror(:,par).*apriori(:,par);
%apriori([1 end],:)
%apriorierror([1 end],:)
end 
