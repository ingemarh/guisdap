% inter3.m: Linear interpolation routine.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% When data (yi) is specified at points (xi) and a new set of points (x)
% is given, the routine interpolates or extrapolates the give the 
% function values (y) at those points (x)
%
% function y=inter3(x,xi,yi);
function y=inter3(x,xi,yi);

y=[];
d=isnan(yi); xi(d)=[]; yi(d)=[];
[xi,ind]=unique(xi); yi=yi(ind);
len=length(xi);
if len<2
  error('No model exists')
elseif len>2
  % linear interpolation and extrapolation
  k=diff(yi)./diff(xi);
  ind=find(x<=xi(2));
  if length(ind)>0, y(ind)=yi(1)+k(1)*(x(ind)-xi(1)); end 
  for i=3:len-1
    ind=find(xi(i-1)<x & x<=xi(i));
    if length(ind)>0, y(ind)=yi(i-1)+k(i-1)*(x(ind)-xi(i-1)); end 
  end  
  ind=find(x>xi(len-1));
  if length(ind)>0, y(ind)=yi(len-1)+k(len-1)*(x(ind)-xi(len-1)); end  
end
