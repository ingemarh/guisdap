% wind.m: the index function of a subset x of a vector y of the same type;
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% y is supposed to consist of distinct elements. The output is a row
% vector.
%
% z=wind(x,y)
%
  function [z,zz]=wind(x,y)
%
  z=[];zz=[]; 
  sy=sort(col(y));dsy=diff(sy);
  sx=sort(col(x));dsx=diff(sx);
  if isempty(dsy==0), error('y not an index'), end
%  if ~isempty(sx==0), error('x not an index'), end
  if length(x)>length(y), error('wrong order'), end
  for i=row(x) 
    w=find(y==i);
    z=[z w];
    zz=[zz, w(1)];
  end
