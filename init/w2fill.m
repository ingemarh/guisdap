% w2fill.m: appends zero boundaries for drawing
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% fills the non-zero outcome [W,lags,ranges] with an appropriate
% number of zero boundaries. (Good for any similar set).
%
% See also: w1fill wind
% function [Wout,lout,rout]=w2fill(Win,lin,rin)
  function [Wout,lout,rout]=w2fill(Win,lin,rin)
%
  ro=w1fill(rin);lo=w1fill(lin);                     %column vectors
  indr=wind(rin,ro);lr=length(ro);
  indl=wind(lin,lo);ll=length(lo);
  Wout=zeros(lr,ll);
  Wout(indr,indl)=Win;
  rout=ro;lout=lo;
