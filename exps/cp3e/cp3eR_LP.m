%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.50       Helsinki 17 January 1994                    %
%   copyright  Asko Huuskonen, Markku Lehtinen                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addrs=42; % Starting address for the best signal acf
addrb=105; % Starting address for background
addrc=147; % Starting address for calibration
gatings=30; % Number of crossed products averages for the signal zero lags
gatingb=260; % Number of products averaged for background (calibration) lags
Nlags=21; % Number of lags
Ngatesb=2; % Number of background gates
Ngatesc=1; % Number of calibration gates

CORR_remote(addrs,addrb,addrc,gatings,gatingb,Nlags,Ngatesb,Ngatesc)

clear addrs addrb addrc gatings gatingb Nlags Ngatesb Ngatesc