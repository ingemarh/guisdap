% CORR_pp.m: defines lag profiles for power profile measurements. 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The function calls COR_pp and hides parameters like the result memory addresses and 
% virtual channel numbers from the user.
%
% input parameters  
% N_gates : Number of gates to be calculated
% gating  : Number of crossed products added to the same result memory location
%
% See also: design, COR_pp
%
% function CORR_pp(N_gates,gating)
  function CORR_pp(N_gates,gating)

global ra_next vc_number type_next code_next

ra=ra_next;
ri=1;
vc=vc_number;
lptype=type_next;
N_skipped=0;
code=code_next;
COR_pp(ra,ri,vc,lptype,gating,N_gates,N_skipped,code)
