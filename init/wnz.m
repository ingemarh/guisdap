% wnz.m: finds non-zero supports in columns of a matrix
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% The interval between upper and lower boundaries of the non-zero
% elements of a column 'ch' in a matrix 'A'
%  function resi=wnz(A,ch)
  function resi=wnz(A,ch)
%
  B=A(:,ch);
  ivector=find(B~=0);
  resi=(min(ivector):max(ivector))';
