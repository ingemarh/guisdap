% res=cheq(x): utility to check equality of components of x. Some slack allowed.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function res=cheq(x)
function res=cheq(x)
maxim=max(x);minim=min(x);
if((maxim-minim)<=1000*eps*max(abs([1,maxim,minim]))),
  res=x(1);
else
  fprintf(' The input values to cheq are not all equal\n')
  res=mean(x);
% disp(x), dbstop error,
% error(' ')
end
