% bitwiseor(x,y,bits): Binary bitwiseor utility function
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Modified by Tony 16.5.97 to avoid error on x==y when y is empty

  function ans=bitwiseor(x,y,bits)

if ~isempty(y) & x==y,
  ans=x;
else
ans=0;
  for i=0:bits-1,
    apu=2^i;
    if (rem(floor(x/apu),2) | rem(floor(y/apu),2));
      ans=ans+apu;
    end 
  end
end
