% get_ADDRSHIFT.m: utility to handle different addressing conventions
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% ADDR_SHIFT is needed as indices to Matlab matrices start from 1 and the first address
% used by the radar can be diffrent.  For instance it is 0 for the EISCAT radars
% GUISDAP uses the true result memory addresses, until a reference to a Matlab-matrix 
% is needed. At that point ADDR_SHIFT is added to the address.
% The routine assumes that the base address is zero. If that is not the case the
% value of the base address must be supplied using the global parameter p_baseaddr
% This parameter is meaningful only for data analysis
%
% function get_ADDRSHIFT.m
function get_ADDRSHIFT

global ADDR_SHIFT p_baseaddr

if length(p_baseaddr)==0,
  p_baseaddr=0; % This is the first result memory address used by EISCAT
end
ADDR_SHIFT=max(0,1-p_baseaddr);
