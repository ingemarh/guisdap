% CORR_lp.m: defines lag profiles for GEN-system long pulses
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The function calls COR_lp and hides parameters like the result memory addresses and 
% virtual channel numbers from the user.
%
% Input parameters:
% gating : number of crossed products added together for the lag zero
% overlap: number of products skipped between gates (positive skipping)
% N_gates: Number of gates
% lags   : Lag values in us
%
% See also: design, COR_lp
%
% function CORR_lp(gating,overlap,N_gates,lags)
  function CORR_lp(gating,overlap,N_gates,lags)

global ra_next vc_number type_next code_next

ra=ra_next;
ri=length(lags);
vc=vc_number;
lptype=type_next;
code=code_next;
COR_lp(ra,ri,vc,lptype,gating,overlap,N_gates,lags,code)
