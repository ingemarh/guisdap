% penv(vc,t): value of effective pulseform for virtual channel 'vc' and time instant 't'
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% This if the preferred way of referencing matrix vc_penv, which contains the
% functions at offsetted time values.
% Parameters
% vc : virtual channel numbers
% t: time instants, any value permitted
% 
% See also: env
% function res=penv(vc,t);
function res=penv(vc,t);

global vc_penv vc_penvo

[len,hups]=size(vc_penv);
t=t-vc_penvo(vc);
iin=find(t>=1 & t<=len);
if length(iin)>0,
  res=zeros(size(t));
  res(iin)=vc_penv(t(iin),vc);
else
  res=[];
end
