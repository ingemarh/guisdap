% Ap(vch,t): ACF of receiver impulse response for virtual channels 'vch' and time lags 't'
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% This if the preferred way of referencing matrix vc_Ap, which contains the function
% for only non-negative values of 't'. Also possible references beyond the matrix
% index limits are hanled. The function also takes care of the fact that
% the value of ACF at lag 0 is stored at Matlab matrix index 1.
% Parameters
% vch : virtual channel numbers
% t: lag values, any value permitted
% 
% See also: Aenv Apenv
%
% function res=Ap(vch,t);
function res=Ap(vch,t);

global vc_Ap

t=abs(t(:))+1;
[len,hups]=size(vc_Ap);
ii=find(t>len);
t(ii)=len*ones(length(ii),1);
res=vc_Ap(t,vch);
