% altcod: generates alternating codes from a Walsh index set
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function to generate alternating codes
% both inputs may be vectors and the output will be a matrix with
% code sequences in its columns
%
% res=altcod(scan,index)
%
  function res=altcod(scan,index)
%
  maxindex=max([scan(:);index(:)]);
  maxin=ceil(log(maxindex)/log(2)); % Log 2 of smallest power of 2 greater than maxindex
  maxindex=2^maxin;
  x=(0:maxindex-1)'/maxindex; s=zeros(maxindex,maxin);
  for i=1:maxin 
    s(:,i)=(-1).^(floor(x*2^i));    % generate binary representations
  end                               % of integers
  s=(1-s)/2;
  res=zeros(length(index),length(scan));
  for i=1:length(scan) 
    for j=1:length(index) 
      res(j,i)=(-1)^sum(s(index(j)+1,:).*s(scan(i)+1,:));
    end 
  end
