% row.m : makes a vector or a matrix into a row vector
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
%
% res=row(A)
%
  function res=row(A)
%
  res=A(:).';
