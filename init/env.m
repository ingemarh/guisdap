% env(vc,t): value of pulseform for virtual channel 'vc' and time instant 't'
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% This if the preferred way of referencing matrix vc_env, which contains the
% functions at offsetted time values.
% Parameters
% vc : virtual channel numbers
% t: time instants, any value permitted
% 
% See also: penv
% function res=env(vc,t);
function res=env(vc,t);

global vc_env vc_envo

[len,hups]=size(vc_env);
t=t-vc_envo(vc);
iin=find(t>=1 & t<=len);
if length(iin)>0,
  res=zeros(size(t));
  res(iin)=vc_env(t(iin),vc);
else
  res=[];
end
