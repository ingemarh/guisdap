% COR_pp.m: calculates the lag profile parameters for power profiles. 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The call is identical to
% calling COR_uprog with the same input parameters and with lags=0;
%
% User specified input parameters:
% ra      : First result memory address to use
% ri      : result memory address increment
% vc      : virtual channel number
% type    : lag profile type, all allowed 's','b','c','o','x'
% gating  : number of successive crossed products added to the same memory location
% N_gates : number of gates for zero lag, the number will be smaller for longer lags
% N_skipped : number of samples skipped at the beginning of the sample vector (often =0)
% code     : user specified code number
% Output is stored in the lp_xxx parameters (global)
%
% See also: COR_uprog
%
% The function calls the uniprog routine.
% function COR_pp(ra,ri,vc,type,gating,N_gates,N_skipped,code)
  function COR_pp(ra,ri,vc,type,gating,N_gates,N_skipped,code)

 lags=0;
 COR_uprog(ra,ri,vc,type,gating,N_gates,lags,N_skipped,code)
