% w2uni.m: The sorted union of two vectors x and y of the same type.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The vectors x and y are assumed to be sorted in ascending 
% order.
% The output is a vector of the same type as the input.
%
% z=w2uni(x,y)
%
  function z=w2uni(x,y)
%
  if isempty(x), z=y;
  elseif isempty(y), z=x;
  return;
  end;
%
  nx=diff(size(x));ny=diff(size(y));
%
  if (nx<0 | ny<0) | (nx==0 & ny==0) 
    m=1;                       % make row vectors
    xx=x';yy=y';
  else
%   m=0;
    xx=x;yy=y;
  end 
  w=sort([xx yy]');            % column vector
  dw=diff(w);
  wi=find(dw==0);
  w(wi)=[];

  if m 
    z=w;
  else
    z=w';
  end
