% vec2covm.m: utility to make a cov matrix out of vector representation
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% creates the covariance matrix back from the storage vector form
% Needed, as the matlab matrix elements cannot be matrices themselves
%
% See also: covm2vec
%function errmatr=vec2covm(covvec)
 function errmatr=vec2covm(covvec)

 len=floor(sqrt(2*length(covvec)));
 errm=zeros(len,len);
 er=covvec(1:len)';
 ind=len;
 errm=errm+diag(ones(len,1),0);
 for i=1:len-1
   errm=errm+diag(covvec(ind+(1:(len-i))),i);
   errm=errm+diag(covvec(ind+(1:(len-i))),-i);   
   ind=ind+len-i;
 end
 
 errmatr=errm.*(er*er');  
 