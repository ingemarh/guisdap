% Walsh_index: Gives the Walsh indices of an alternating code set
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% Function to give the Walsh indices of an alternating code set
% Nbits : number of bits in the code
% index : indices specifying the alternating code set in decimal base
% NC    : number of codes in the complete set
% function [index,NC]=Walsh_index(Nbits);
  function [index,NC]=Walsh_index(Nbits);
		
		
% the original Walsh index definitions are in octal base
% because he original references give the indices so
if Nbits>32 & Nbits<=64
  octal=[0 1 2 4 10 20 40 100 137 140 36 75 172 53 127 ...
         161 74 171 55 133 151 14 31 62 144 27 57 136 ...
         143 30 61 142 33 67 156 3 7 16 34 70 160 77 ...
         177 41 103 131 154 6 15 32 64 150 17 37 76 ...
         174 47 117 101 134 146 22 45 112]; NC=128;
elseif Nbits>16 & Nbits<=32
	 octal=[0 1 2 4 10 20 40 30 61 72 55 3 7 16 34 70 50 11 23 ...
		       46 24 51 12 25 52 14 31 62 74 41 33 67]; NC=64;
elseif Nbits>8 & Nbits<=16
  octal=[0 1 2 4 10 20 36 3 7 16 34 6 15 32 12 25]; NC=32;
elseif Nbits>4 & Nbits<=8
  octal=[0 1 2 4 10 3 7 16]; NC=16;  
elseif Nbits>2 & Nbits<=4
  octal=[0 1 2 4]; NC=8;  
else
  fprintf(' No Walsh index set available for %.0f bits\n',Nbits)
		error(' ')
end

% Change from octal to decimal numbers
index=zeros(size(octal));
mult=1;
while any(octal>0)
  index=index+rem(octal,10)*mult;
  mult=mult*8;
  octal=floor(octal/10);
end

index=index(1:Nbits);
