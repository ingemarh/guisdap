% lin_filt.m: Interpolates the linear phase filter impulse response function
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% from the values measured by Jussi Markkanen
% IH:All Jussis values
% input parameters:
% BW     : desired bandwidth in kHz (EISCAT definition)
% ch     : specifies the channel numbers (optional)
% p_dtau : global time unit in us (global parameter)
% output parameters:
% impresp: the impulse response
% ch_p   : impulse resposes for channels given by ch (global)
%
% See also: but_filt, REC_impresp
%
% function impresp=lin_filt(BW,ch);
  function impresp=lin_filt(BW,ch);
  
  global ch_p p_dtau
% 
% measured impulse response of a 35.4 kHz linear phase filter.
% values given for each us.
 p_m=[4 3 6.5 18.5 46 91 150.5 220 290 353.5 401 429.5 435 419 384.5 336.5 ...
 280 221 164.5 114 72 39.5 16.5 2.5 -5.5 -7.5 -6.5 -4 -1 2 3.5 5 5 4.5 3 2 1 ...
 0 -0.5 0.5 0 0.5]';

  len = length(p_m);

  dt  = 35.4/BW; % time step for the desired bandwidth

% interpolation
  p_mx= ((0:(len-1))*dt)';
  px  = (0:p_dtau:p_mx(len))';
  p = spline(p_mx,p_m,px);
  
% scale to get unit area and
% assign to all channels specified at input

  impresp=p/sum(p);
  if nargin==2,
   ch_p(1:length(px),ch) = impresp*ones(1,length(ch));
  end
