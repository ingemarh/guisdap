% wl.m: short form of the reduced lag ambiguity function
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% vc:   virtual channel number
% lag: lag value
% w : lag ambiguity function
% l : lag ambiguity function support
%     plot(l*p_dtau,w) shows the function with correct lag values in us
%
% See also: lpgwom wr Ap Aenv
%
%  function [w,l]=wl(vc,lag)
  function [w,l]=wl(vc,lag)

global vc_Ap

len=length(vc_Ap(:,vc));
l=((lag-len+1):(lag+len-1))';
w=Ap(vc,l-lag).*Aenv(vc,l);
