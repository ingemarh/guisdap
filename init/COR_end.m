% COR_end.m: Function to cut the unused part of the lp_XXX parameters. 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Called automatically by design
%
% See also: design, COR_init
%
% function COR_end
function COR_end

global lp_t1 lp_t2 lp_h lp_ra lp_nfir lp_fir lp_dec lp_T lp_dt 
global lp_nt lp_vc lp_ri lp_bcs lp_code lp_ind 
global ra_next ra_prev bm_next vc_next

len=length(lp_t1);
if lp_ind<=len
  lp_t1(lp_ind+1:len)=[];
  lp_t2(lp_ind+1:len)=[];
  lp_h(lp_ind+1:len)=[];
  lp_ra(lp_ind+1:len)=[];
  lp_nfir(lp_ind+1:len)=[];
  lp_dec(lp_ind+1:len)=[];
  lp_fir(:,lp_ind+1:len)=[];
  lp_T(lp_ind+1:len)=[];
  lp_dt(lp_ind+1:len)=[];
  lp_nt(lp_ind+1:len)=[];
  lp_vc(lp_ind+1:len)=[];
  lp_ri(lp_ind+1:len)=[];
  lp_bcs(lp_ind+1:len)=[];
  lp_code(lp_ind+1:len)=[];
end
[M,N]=size(lp_fir);
MA=max(lp_nfir);
if MA<M
  lp_fir(MA+1:M,:)=[];
end
clear lp_ind
