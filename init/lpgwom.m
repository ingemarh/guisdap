% lpgwom.m:  calculates the reduced spectral ambiguity function
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% lpg     : lag profile group number
% womsum  : reduced spectral ambiguity function
%
% See also: lpgwomcalc wl
%
% function womsum=lpgwom(lpg);
function womsum=lpgwom(lpg);

global p_om  p_dtau p_om0 lp_t1 lp_t2 lp_vc lp_nfir lp_fir vc_group
womsum=zeros(size(p_om))';
dt=-5000;wc=-1;
for lp=lpg_lp(lpg),  
 % disp(lp),
  used_oldvalues=0;
  t2=lp_t2(lp);t1=lp_t1(lp);vc=lp_vc(lp);  
  if (dt==t2-t1 & vc==wc) 
    womsum=womsum+sum(lp_fir(1:lp_nfir(lp),lp))*wold; used_oldvalues=1;
  elseif (dt==t2-t1 & wc>0 & vc~=wc)
    if vc_group(vc)==vc_group(wc),
      womsum=womsum+sum(lp_fir(1:lp_nfir(lp),lp))*wold; used_oldvalues=1;
    end
  end  
  if used_oldvalues==0,
    dt=t2-t1;
    [w,wx]=wl(vc,dt);
    ind=find(w~=0); w=w(ind); wx=wx(ind);
    ch=1;  % hyi hyi
    wnew=sum( (w*ones(size(p_om))').*exp( wx*p_om'*(p_dtau*1e-6*p_om0(ch)*sqrt(-1)) ) );
    womsum=womsum+sum(lp_fir(:,lp))*wnew;
    wc=vc;wold=wnew; 
  end
end
