% mrqmn: Least squares fit routine (Marquart-Levenberg method)
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% See also: mrqmndiag
function [aa,chi2,its,alpha,OK]=...
      mrqmn(a,ym,variance,ftol,itmax,kd2,p_coeffg,f_womega,p_om,pldfvv,errorlim)
 
  global p_m0

% comp=a(6);a=a(1:5);
vv=find(variance~=0);
va=find(variance((end-length(a)+1):end)~=0);
  if nargin==10, errorlim=inf; end
  OK=1;
  if min(size(variance))==1, diagonal=1;end
  if diagonal,
    invsigma=1 ./variance(vv);
    apu=invsigma*ones(1,length(va));
  else
    % The apriori variance values for the parameters (especially for collision frequency)
    % are sometimes so low in scaled units, that a simple 
    % invsigma=inv(variance)
    % produces unnecessary warnings about the condition number of the matrix!
    % Therefore we treat the apriori part separately here.
    invsigma=diag(1./diag(variance(vv)));
    [M,N]=size(variance(vv));
%   invsigma(1:M-5,1:M-5)=inv(variance(1:M-5,1:M-5));
    NP=length(va);
    invsigma(1:M-NP,1:M-NP)=inv(variance(1:M-NP,1:M-NP));
  end
  lambda=0.001;
  aa=a; validder=0;
% ya=dirthe([aa,comp],p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0);
  ya=dirthe(aa,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0);
  dyda=zeros(length(ya),length(va));
  if diagonal,
    chi2=(ym(vv)-ya(vv))'*(invsigma.*(ym(vv)-ya(vv)));
  else
    chi2=(ym(vv)-ya(vv))'*invsigma*(ym(vv)-ya(vv));
  end
  its=0;
  while its<itmax 
    if ~validder 
      its=its+1;
      for i=1:length(va)
        aa2=aa; aa2(va(i))=aa(va(i))+0.0001;
%       dyda(:,i)=(ya-dirthe([aa2,comp],p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0))/0.0001; %calculate derivatives;
        dyda(:,i)=(ya-dirthe(aa2,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0))/0.0001; %calculate derivatives;
      end 
      if diagonal,
        alpha=dyda(vv,:)'*(apu.*dyda(vv,:));
        beta=dyda(vv,:)'*(invsigma.*(ya(vv)-ym(vv)));
      else
        alpha=dyda(vv,:)'*invsigma*dyda(vv,:);
        beta=dyda(vv,:)'*invsigma*(ya(vv)-ym(vv));
      end
     if its==1 error=sqrt(diag(inv(alpha)));
       if error(1)>errorlim, OK=2; return,  end
     end
    end 
    da=((alpha+lambda*eye(size(alpha)))\beta)';
%    disp(da);
    chi2old=chi2; yaold=ya; aaold=aa;
    aa(va)=aa(va)+da;
%   ya=dirthe([aa,comp],p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0);
    ya=dirthe(aa,p_coeffg,f_womega,kd2,p_om,pldfvv,p_m0);
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
end 
%aa=[aa,comp];
al=inv(alpha);
alpha=zeros(length(aa)); alpha(va,va)=al;
