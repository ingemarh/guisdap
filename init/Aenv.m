% Aenv(vch,t): ACF of transmitter envelope for virtual channel 'vch' and time lag 't'
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% This if the preferred way of referencing matrix vc_Aenv, which contains the function
% for only non-negative values of 't'. Also possible references beyond the matrix
% index limits are hanled. The function also takes care of the fact that
% the value of ACF at lag 0 is stored at Matlab matrix index 1.
% Parameters
% vch : virtual channel numbers
% t: lag values, any value permitted
% 
% See also: Ap Apenv
%
% function res=Aenv(vch,t);
function res=Aenv(vch,t);

global vc_Aenv

t=abs(t(:))+1;
[len,hups]=size(vc_Aenv);
ii=find(t>len);
t(ii)=len*ones(length(ii),1);
res=vc_Aenv(t,vch);
