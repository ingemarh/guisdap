% CORR_mp: defines lag profiles for decoded multipulse experiments
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The function calls COR_mp and hides parameters like the result memory addresses and 
% virtual channel numbers from the user.
%
% Input parameters:
% gating : Number of successive crossed products added to the same memory location
% N_gates: Number of gates
% lags   : Lag values in us
%
% See also: design, COR_mp
%
% function CORR_mp(gating,N_gates,lags)
  function CORR_mp(gating,N_gates,lags)

global ra_next vc_number type_next code_next

ra=ra_next;
ri=length(lags);
vc=vc_number;
lptype=type_next;
N_skipped=0;
code=code_next;
COR_mp(ra,ri,vc,lptype,gating,N_gates,lags,N_skipped,code)
