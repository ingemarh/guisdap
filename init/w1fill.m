% w1fill.m: appends vectors with zeros for drawing xxx
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% w1fill.m: fills the coordinate column vector zin with an appropriate 
% number of zero boundary coordinates.
% See also: w2fill w2uni
% function zo=w1fill(zi)
  function zo=w1fill(zi)
%
  z1=min(zi)-1;z2=max(zi)+1;
  dz=diff(zi);
  w=find(dz>1);
  if isempty(w), zo=[z1;zi;z2]; return; end
  zr=zi(w)+1;zl=zi(w+1)-1;
  z=w2uni(zr,zl);
  zo=[z1;w2uni(z,zi);z2];
