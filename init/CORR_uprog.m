% CORR_uprog.m: defines the Uniprog lag profiles.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The function calls COR_uprog and hides parameters like the result memory addresses and 
% virtual channel numbers from the user.
%
% input parameters  
% N_gates : Number of gates to be calculated
% gating  : Number of crossed products added to the same result memory location
% lags    : lag values for the profiles in us
%
% See also: design, COR_uprog
%
% function CORR_uprog(N_gates,gating,lags)
  function CORR_uprog(N_gates,gating,lags)

global ra_next vc_number type_next code_next

ra=ra_next;
ri=1;
vc=vc_number;
lptype=type_next;
N_skipped=0;
code=code_next;
COR_uprog(ra,ri,vc,lptype,gating,N_gates,lags,N_skipped,code)
