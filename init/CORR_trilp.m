% CORR_trilp: defines lag profiles for the pre-GEN-system long pulse algorithm
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The function calls COR_trilp and hides parameters like the result memory addresses and 
% virtual channel numbers from the user.
%
% Input parameters:
% gating : number of crossed products added together for the lag zero
% overlap: number of products skipped between gates (positive skipping)
% N_gates: Number of gates
% lags   : Lag values in us
%
% See also: design, COR_trilp
% function CORR_trilp(gating,overlap,N_gates,lags)
  function CORR_trilp(gating,overlap,N_gates,lags)

global ra_next vc_number type_next code_next

ra=ra_next;
ri=length(lags);
vc=vc_number;
lptype=type_next;
code=code_next;
COR_trilp(ra,ri,vc,lptype,gating,overlap,N_gates,lags,code)
