% type2ind.m: utility to interpret lp types
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% gives a number to be used as a matrix index which correspond to
% a given lag profile type (signal, background etc)
% function ind=bcs2ind(type)
  function ind=bcs2ind(type)
  
    if type=='b', ind=1;
elseif type=='c', ind=2;
elseif type=='s', ind=3;
elseif type=='o', ind=3;
elseif type=='x', ind=3;
else
  error(['Unspecified lag profile type used as input (' type ')'])
end
