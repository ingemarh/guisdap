% error_estimate.m: Calculates error estimates for inversion of dirthe
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
  function [error,correl,alpha]=error_estimate(aa,variance,kd2,p_coeffg,f_womega,p_om,pldfvv,fb_womega)
  
  global p_m0
 
% warning_state=warning;
% warning off
  vv=find(variance~=0);
  va=find(variance((end-length(aa)+1):end)~=0);
  if min(size(variance))==1, diagonal=1;end
  if diagonal,
    invsigma=1 ./variance(vv);
    apu=invsigma*ones(1,length(va));
  else
    invsigma=inv(variance(vv));
  end

  ya=dirthe(aa,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0,fb_womega);
  dyda=zeros(length(ya),length(va));
  ag=1:4; as=5; %log!
  errfac=ones(length(aa),1); errfac(ag)=aa(ag); errfac(as)=sqrt(4+aa(as).^2); %log!
  aa(ag)=log(aa(ag)); aa(as)=asinh(aa(as)/2); %log!
   for i=1:length(va) 
    aa2=aa; aa2(va(i))=aa(va(i))+0.0001;
    aa2(ag)=exp(aa2(ag)); aa2(as)=2*sinh(aa2(as)); %log!
    dyda(:,i)=(ya-dirthe(aa2,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0,fb_womega))/0.0001;
   end 
   if diagonal
     alpha=dyda(vv,:)'*(apu.*dyda(vv,:));
   else
     alpha=dyda(vv,:)'*invsigma*dyda(vv,:);
   end
al=inv(alpha);
alpha=zeros(length(aa)); alpha(va,va)=al;
alpha=alpha.*(errfac*errfac'); %log!
error=sqrt(diag(alpha))';
correl=alpha./(error'*error+eps);
%warning(warning_state);
