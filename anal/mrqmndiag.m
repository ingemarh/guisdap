% mrqmndiag.m:  non-linear iteration routine of Marquart-Levenburg type
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% input arguments:
% a        : parameters to fit
% ym       : measurements
% variance : data variance, either diagonal of full
% ftol     : stop execution, when step for all paramters less than ftol
% itmax    : maximum number of iterations
% kd2 p_coeffg, f_womega, p_om, pldfvv : parameters need by dirthe 
% Output parameters
% aa    : The least-squares point
% chi2  : residual of the fit
% its   : number of iterations
% alpha : the error covariance matrix at the second last iteration point 
%
% See also: dirthe, mrqmn
%  function [aa,chi2,its,alpha]=mrqmndiag(a,ym,variance,ftol,itmax,kd2,p_coeffg,f_womega,p_om,pldfvv,p_m0)
function [aa,chi2,its,alpha]=mrqmndiag(a,ym,variance,ftol,itmax,kd2,p_coeffg,f_womega,p_om,pldfvv,p_m0,physlim,fb_womega)
 
% warning_state=warning;
% warning off
  vv=find(variance~=0);
  va=find(variance((end-length(a)+1):end)~=0);
  if min(size(variance))==1, diagonal=1;end
  if diagonal,
    invsigma=1 ./variance(vv);
    apu=invsigma*ones(1,length(va));
  else
    invsigma=inv(variance(vv));
  end
  lambda=0.001;
  aa=a; validder=0;
  ya=dirthe(aa,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0,fb_womega);
  dyda=zeros(length(ya),length(va));
  if diagonal,
    chi2=(ym(vv)-ya(vv))'*(invsigma.*(ym(vv)-ya(vv)));
  else
    chi2=(ym(vv)-ya(vv))'*invsigma*(ym(vv)-ya(vv));
  end
  its=0;
  % fit logs
  ag=1:4; as=[5 (1:2)+4+length(p_m0)];
  aa(ag)=log(aa(ag)); aa(as)=asinh(aa(as)/2); %log!
  physlim(:,ag)=log(physlim(:,ag)); physlim(:,as)=asinh(physlim(:,as)/2); %log!
% [aa;variance((end-length(a)+1):end)']
  while its<itmax
    if ~validder 
      its=its+1;
      for i=1:length(va) 
        aa2=aa; aa2(va(i))=aa(va(i))+0.0001;
        aa2(ag)=exp(aa2(ag)); aa2(as)=2*sinh(aa2(as)); %log!
        dyda(:,i)=(ya-dirthe(aa2,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0,fb_womega))/0.0001; %calculate derivatives;
      end 
      if diagonal,
        alpha=dyda(vv,:)'*(apu.*dyda(vv,:));
        beta=dyda(vv,:)'*(invsigma.*(ya(vv)-ym(vv)));
      else
        alpha=dyda(vv,:)'*invsigma*dyda(vv,:);
        beta=dyda(vv,:)'*invsigma*(ya(vv)-ym(vv));
      end
    end 
    da=((alpha+lambda*eye(size(alpha)))\beta)';
%    disp(da);
    chi2old=chi2; yaold=ya; aaold=aa;
    aa(va)=aa(va)+da;
    d=find(aa(va)<physlim(1,va)); aa(va(d))=physlim(1,va(d));
    d=find(aa(va)>physlim(2,va)); aa(va(d))=physlim(2,va(d));
%   da=aa(va)-aaold(va);
  aa2=aa; aa2(ag)=exp(aa2(ag)); aa2(as)=2*sinh(aa2(as)); %log!
    ya=dirthe(aa2,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0,fb_womega);
    if diagonal,
      chi2=(ym(vv)-ya(vv))'*(invsigma.*(ym(vv)-ya(vv)));
    else
      chi2=(ym(vv)-ya(vv))'*invsigma*(ym(vv)-ya(vv));
    end
    if chi2>=chi2old 
      chi2=chi2old; aa=aaold; ya=yaold; lambda=lambda*10; validder=1;
      if max(abs(da))<ftol, OK=0; break, end 
    else 
      lambda=lambda/10; validder=0;
      if max(abs(da))<ftol, OK=0; break, end 
    end 
  end 
al=inv(alpha);
alpha=zeros(length(aa)); alpha(va,va)=al;
aa(ag)=exp(aa(ag)); aa(as)=2*sinh(aa(as)); %log
aa2=ones(length(aa),1); aa2(ag)=aa(ag); aa2(as)=sqrt(4+aa(as).^2); %log
alpha=alpha.*(aa2*aa2');
%warning(warning_state);
