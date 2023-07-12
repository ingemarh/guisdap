%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   GUP ver. 1.32       Helsinki 6 March 1992                       %
%   copyright  Asko Huuskonen, Markku Lehtinen                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ERRRIS2vcinit.m

tphase=3179;

% 1- alters
 vc_ch=5*ones(1,32);
 vc_t1=0:3:95;
 vc_ch=[vc_ch 6*ones(1,32)];
 vc_t1=[vc_t1 1:3:95];
 vc_ch=[vc_ch 7*ones(1,32)];
 vc_t1=[vc_t1 2:3:95];

vc_t1=vc_t1*tphase;
vc_t2=vc_t1+tphase;

