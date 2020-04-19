% box_filt.m: Interpolates the boxcar filter impulse response function
% GUISDAP v.8.9 16-09-27
%
% input parameters:
% width  : box lengths in us
% p_dtau : global time unit in us (global parameter)
% output parameters:
% impresp: the impulse response scaled to get unit area
%
% See also: but_filt, REC_impresp
%
% function impresp=box_filt(width);
function impresp=box_filt(width);
global p_dtau
wp=round(width/p_dtau);
for i=1:length(wp)
 impresp(:,i)=ones(wp,1)/wp;
end
