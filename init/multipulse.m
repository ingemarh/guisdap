% multipulse: Finds the lags a multipulse sequence produces
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% From the multipulse sequence vector V finds the lags the
% sequence produces; for a good multipulse, there are no
% repetitions. This is SSU (system sum). 
%
% function ssu=multipulse(V)
  function ssu=multipulse(V)


if length(V)==1 % Code given in one integer, i.e. 3152 for 5-pulse code
  code=[];
  while V>0,
    code=[code,rem(V,10)];
    V=floor(V/10);
  end
else
  code=V;
end

n=length(code);m=n*(n+1)/2;
ssu=zeros(1,m);k=0;
for i=1:n,
  for j=1:n-i+1,k=k+1;
    ssu(k)=sum(code(j:i+j-1));
  end
end
ssu=sort(ssu);

