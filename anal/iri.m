%%%%%%%%function varargout = iri(varargin)
%par=iri2001(fields,tim,pos,h)
% INPUT:  pos(lat,long)  LATITUDE NORTH AND LONGITUDE EAST IN DEGREES 69.2,19.2
% 	  tim(tsec,year) eg [0 1996] for 960101 00:00 ut
%         H(hstart,hstop,hstep)  HEIGHT RANGE IN KM; maximal 100 heights 100,590
%
%   	  fields 1  ELECTRON DENSITY/M-3     [1 4 3]
%   		2  NEUTRAL TEMPERATURE/K
%   		3  ION TEMPERATURE/K
%   		4  ELECTRON TEMPERATURE/K
%   		5  O+ ION DENSITY/M-3
%   		6  H+ ION DENSITY/M-3
%   		7  HE+ ION DENSITY/M-3
%   		8  O2+ ION DENSITY/M-3
%   		9  NO+ ION DENSITY/M-3
%              12  HEIGHTS
%*****************************************************************
%*** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        ***
%***     ELECTRON DENSITY         60/80 KM       1000 KM       ***
%***     TEMPERATURES              120 KM        3000 KM       ***
%***     ION DENSITIES             100 KM        1000 KM       ***
%*****************************************************************
%%%%%%%%# mex
%%%%%%%error('C-MEX function not found');
%%%%%%%% [EOF] iri.m
