%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.32       Helsinki 6 March 1992                       %
%   copyright  Asko Huuskonen, Markku Lehtinen                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ionc3Tvcinit.m

tphase=3*3179;

% 1- alters
 vc_ch=[3*ones(1,32) 4*ones(1,32) 5*ones(1,32)];
 vc_t1=[0:31 0:31 0:31];
 vc_t2=[1:32 1:32 1:32];

% 2- pps
% vc_ch=[vc_ch vc_ch];
% vc_t1=[vc_t1 vc_t1];
% vc_t2=[vc_t2 vc_t2];

vc_t1=vc_t1*tphase;
vc_t2=vc_t2*tphase;

