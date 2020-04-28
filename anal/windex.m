% windex.m: the index function of a subset x of a vector y of the same type;
% GUISDAP v.9.1 20-05-01 Copyright EISCAT, Asko Huuskonen and Markku Lehtinen
%
% y is supposed to consist of distinct elements. The output is a row
% vector.
%
% z=windex(x,y)
function [z,zz]=windex(x,y)
z=[];zz=[]; 
sy=sort(col(y));dsy=diff(sy);
sx=sort(col(x));dsx=diff(sx);
if ~isempty(dsy) && isempty(dsy==0), error('GUISDAP:windex','y not an index'), end
if length(x)>length(y), error('GUISDAP:windex','wrong order'), end
for i=row(x) 
  w=find(y==i);
  z=[z w];
  zz=[zz w(1)];
end
