% lpg_addr.m: returns the result memory addresses belonging to a lag profile group. 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function addr=lpg_addr(lpgs)

function addr=lpg_addr(lpgs)

global lpg_ra lpg_nt lpg_ri

addr=[];
for lpg=lpgs
 addr=[addr,lpg_ra(lpg)+(0:lpg_nt(lpg)-1)*lpg_ri(lpg)];
end
