% covm2vec.m: creates a vector from the error covariance matrix.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% creates a vector from the error covariance matrix. The vector contains
% - errors (sqrt of the diagonal elements, first 'len' elements)
% - correlations so that first side diagonal comes first etc
% Needed, as the matlab matrix elements cannot be matrices themselves
%
% See also: vec2covm
%function covvec=covm2vec(errmatr)
function covvec=covm2vec(errmatr)

er=sqrt(diag(errmatr));  %These are the errors of the parameters
d=find(er~=0);
covm=errmatr;
covm(d,d)=errmatr(d,d)./(er(d)*er(d)'); %These are the covariances

len=length(er);
covvec=zeros(1,len*(len+1)/2);
covvec(1:len)=er';
ind=len;
for i=1:len-1
  covvec(ind+(1:(len-i)))=diag(covm,i)';
  ind=ind+len-i;
end
