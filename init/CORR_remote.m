% CORR_remote.m: lag profiles for the GENremote correlator program
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%
% This function produces the lag profiles for the GENremote correlator
% program by T.Turunen with a minimum number of input arguments
% input parameters 
% addrs  : Starting address for signal, specify the best one (third normally)
% addrb  : starting address for background
% addrc  : starting address for calibration
% gatings: Number of crossed products averaged for signal zero lag
% gatingb: Number of crossed products averaged for background or calibration
% Ngatesb: Number of background gates
% Ngatesc: Number of calibration gates
% Nlags  : Number of lags
%
% function CORR_remote(addrs,addrb,addrc,gatings,gatingb,Nlags,Ngatesb,Ngatesc)
  function CORR_remote(addrs,addrb,addrc,gatings,gatingb,Nlags,Ngatesb,Ngatesc)

global vc_ch vc_adcint p_dtau

lags=(0:Nlags-1)*cheq(vc_adcint)*p_dtau;

Ngatess=1;
if nargin<7,Ngatesb=1;end
if nargin<8,Ngatesc=1;end

COR_init(length(vc_adcint)*(2*length(lags)+1),length(lags))

for vc=1:length(vc_adcint),
  COR_trilp(addrs,Nlags,vc,'s',gatings, 0,Ngatess,lags,1)
    COR_box(addrb,Nlags,vc,'b',gatingb, 0,Ngatesb,lags,1)
    COR_box(addrc,Nlags,vc,'c',gatingb, 0,Ngatesc,0,1)
end

COR_end
