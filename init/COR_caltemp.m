% COR_caltemp.m : sets the calibration temperature
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function Temp=COR_caltemp(type)

function Temp=COR_caltemp(type)

global p_calTemp

if type=='c' | type=='C'  % Calibration measurement
  Temp=p_calTemp;
else
  Temp=0;
end
