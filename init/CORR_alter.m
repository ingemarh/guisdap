% CORR_alter: Lag profiles for decoded alternating code experiments
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% CORR_alter defines lag profiles for decoded alternating code experiments. 
% The function calls COR_alter and hides parameters like the result memory addresses and 
% virtual channel numbers from the user.
%
% input parameters  
% N_gates : Number of gates to be specified
% gating  : Number of crossed products added to the same result memory location
% lags    : lag values for the profiles in us
% Nbits   : Number of bits in the alternating code
% bitsep  : distance of leading edges of code bauds (optional).
%           If not specified, the value will be equal to the bit length
%           This must be specified for e.g. interlaced alternating codes
%
% See also: design, COR_alter
%
% function CORR_alter(N_gates,gating,lags,Nbits,bitsep)
  function CORR_alter(N_gates,gating,lags,Nbits,bitsep)

global ra_next vc_number type_next code_next vc_env p_dtau

ri=length(lags);
vc=vc_number;
lptype=type_next;
if nargin==4, bitsep=length(wnz(vc_env,vc))*p_dtau/Nbits; end
N_skipped=0;
code=code_next;
if all(rem(lags,bitsep)==0) % & gating>1
  ra=ra_next+(0:(length(lags)-1));
  COR_alter(ra,ri,vc,lptype,gating,N_gates,Nbits,bitsep,lags,N_skipped,code)
elseif gating==1 % any(rem(lags,bitsep)~=0) & gating==1
  ra=ra_next;
  COR_fraclp(ra,vc,lptype,N_gates,Nbits,bitsep,lags,code)
else
  fprintf(' No correlator definition routine for fractional lags with gating exists\n')
end
