% Barker.m: gives the various Barker codes
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% function to give various Barker codes
%
% function code=Barker(Nbits);
function code=Barker(Nbits);

if Nbits==3
  code=[1 1 -1]';
elseif Nbits==4
  code=[1 1 -1 1]';
elseif Nbits==5
  code=[1 1 1 -1 1]';
elseif Nbits==7
  code=[1 1 1 -1 -1 1 -1]';
elseif Nbits==11
  code=[1 1 1 -1 -1 -1 1 -1 -1 1 -1]';
elseif Nbits==13
  code=[1 1 1 1 1 -1 -1 1 1 -1 1 -1 1]';
elseif exist('FIRcode')
  code=col(FIRcode(Nbits));
else
  error(sprintf('%.0f bit Barker code not available',Nbits))
end
