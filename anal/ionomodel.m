% ionomodel.m: user-supplied ionospheric model
%included ionomodel_control==6 aposteriori comp values
% GUISDAP v.1.80 01-07-27 Copyright EISCAT, Huuskonen&Lehtinen
% 
% 'ionomodel' is a user-supplied function which for a given set of heights
% outputs the plasma parameters (Ne, Ti, Te/Ti, coll, [O+]/Ne, velocity)
% with their a priori uncertainties. This simple example is static, a more complete
% model might use the date and time values to modify the model.
%
% function [apriori, apriorierror]=ionomodel(heights);

function [apriori, apriorierror]=ionomodel(heights);

global ionomodel_control iono_model p_m0 fit_altitude

len=length(heights);
heights=col(heights);

if nargout==2, error=1; else, error=0; end
if isempty(ionomodel_control), ionomodel_control=0; end
if isempty(iono_model), iono_model='giveme'; end
if any([7 6]-size(fit_altitude)>0)
 ff=fit_altitude;
 % Altitudes (h2>h1) where to fit each parameter resp
 % Strucure: h1 h2 hd(transition height) maxerr minerr relerr(relative or static errors)
 fit_altitude=[0 Inf 0 1e3 0 1
               0 Inf 0 1e4 0 0
             107 Inf 0 1e1 0 0
               0 107 0 1e2 0 1
               0 Inf 0 1e5 0 0
               0   0 0   1 0 0
               0   0 0   1 0 0];
%fit_altitude(:,4)=[1e-3;.1;1e-3;1e-2;1e-3;1e-3;1e-3];
 fit_altitude(1:size(ff,1),1:size(ff,2))=ff;
end

model=['ionomodel_' iono_model];
if exist([model '.m'])
 [altitude,ne,te,ti,coll,cO,cM2,cH]=feval(model,heights);
else
 error(['Undefined ionospheric model: ' iono_model ' (giveme,gup150,iri)']);
end
heights2=heights;
heights=max(heights,altitude(1));
heights=min(heights,altitude(end));


nion=length(p_m0);
apriori=ones(len,4+nion);
% Electron density
apriori(:,1)=exp(inter3(heights2,altitude,log(ne)))';

% Ion temperature
apriori(:,2)=inter3(heights,altitude,ti)';
 
% Temperature ratio
apriori(:,3)=inter3(heights,altitude,te)'./apriori(:,2);
 
% Ion-neutral collision frequency
apriori(:,4)=inter3(heights,altitude,coll)';
 
% Ion velocity
apriori(:,5)=zeros(len,1);

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

if error
 apriorierror=ones(size(apriori));
 for par=1:size(apriori,2)
  h1=fit_altitude(par,1); h2=fit_altitude(par,2); hd=fit_altitude(par,3)+eps;
  err=(tanh((heights-h1)/hd)+tanh((h2-heights)/hd))/2;
  apriorierror(:,par)=err*fit_altitude(par,4)+fit_altitude(par,5);
  if fit_altitude(par,6)
    apriorierror(:,par)=apriorierror(:,par).*apriori(:,par);
  end 
 end
%apriori([1 end],:)
%apriorierror([1 end],:)
end 
