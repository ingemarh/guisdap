% XMIT_sp.m: Function to define a simple pulse (long or short)
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% input parameters
% ch          : physical channel for the transmission
% starttime   : start time of the transmitted envelope in us
% pulselength : length of the pulse in us
%
% See also: XMIT_pulse
%
% function XMIT_sp(ch,starttime,pulselength)
  function XMIT_sp(ch,starttime,pulselength)
  
XMIT_pulse(ch,starttime,1,1,pulselength)
