function t=taper(n)
t=cos(((1:n)'-(n+1)/2)*pi/n).^.25;
