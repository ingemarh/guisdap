% plot_w2.m: plots 2-dim range-lag ambiguity functions
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% plot program to show the two-dimensional range-lag ambiguity function with
% the reduced ambiguity functions for virtual channel 'vc' and sampling times
% 't1' and 't2'
%
% See also w2om
%
% function plot_w2(vc,t1,t2)

function plot_w2(vc,t1,t2)

[ww,wS,om,ww2,wL]=w2om(vc,t1,t2);

clf
axes('Pos',[.35,.45,.5,.5])
[M,N]=size(ww2);
sM=1;ceil(M/50);
sN=1;ceil(N/50);
mesh(wL(1:sN:N),wS(1:sM:M),ww2(1:sM:M,1:sN:N))
ax=axis;
axis([wL(1),wL(N),wS(1),wS(M),ax(5:6)])
view(-45,30)
ylabel('Range [us]')
xlabel('Lag [us]')
title('Two-dimensional range-lag ambiguity function')
ax=axis;
%axes('Pos',[.1,.35,.15,.6])
axes('Pos',[.1,.29,.15,.3])
wwS=sum(ww2');
plot(wwS,wS)
axis([min([wwS,0]),max([wwS,0]),ax(3:4)])
title('Range amb. function')
ylabel('Range [us]')
%axes('Pos',[.35,.1,.6,.15])
axes('Pos',[.6,.15,.3,.15])
wwL=sum(ww2);
plot(wL,wwL)
axis([ax(1:2),min([wwL,0]),max([wwL,0])])
title('Lag amb. function')
xlabel('Lag [us]')
