% cor=autocorr(data) : Calculates autocorrelation function of columns of data
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
function cor=autocorr(data)

[m n]=size(data);
check=0;

if (m==1)
	data=data';
	check=1;
end;
cor=conv(data,flipud(data));

if (check==1)
	cor=cor';
end;



