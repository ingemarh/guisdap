% findrg.m : Finds all lag profiles contributing to a result memory location m
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%
% [lp,t1,t2,vc]=findrg(m)
%
  function [lp,t1,t2,vc]=findrg(m)
%

  global lp_ra lp_nt lp_ri lp_t1 lp_dt lp_t2 lp_vc

  lp=find( lp_ra<=m & m<=lp_ra+((lp_nt-1).*lp_ri) ...
            & round((m-lp_ra)./lp_ri)==(m-lp_ra)./lp_ri );
  if nargout>1 
    t1=lp_t1(lp)+lp_dt(lp).*(m-lp_ra(lp))./lp_ri(lp);
    t2=t1+lp_t2(lp)-lp_t1(lp);
    vc=lp_vc(lp);
  end
