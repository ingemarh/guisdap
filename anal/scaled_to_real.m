% scaled_to_real.m: transforms scaled plasma parameters to physical units 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Parameters:
% physical: plasma parameters in physical units
% scaled: plasma parameters in scaled units
% See also: real_to_scaled
%function physical=scaled_to_real(scaled)
function physical=scaled_to_real(scaled)

global p_N0 p_m0 p_T0 p_om0 k_radar0

physical=scaled; % affects element 3 and also 6 (if specified on input)
physical(:,1)=scaled(:,1)*p_N0;
physical(:,2)=scaled(:,2).*p_T0;
ch=1;  % hyi hyi
physical(:,4)=scaled(:,4)*(p_om0(ch));
physical(:,5)=scaled(:,5)*(p_om0(ch)/k_radar0(ch));
