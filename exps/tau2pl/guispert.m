% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='T'
  lp=364;
  g=gainfit(d_data((1:lp)+17134),10);
  g=g/g(end);
  d_data((1:lp)+17134)=d_data((1:lp)+17134)./g;
%%g=gainfit(d_data((1:lp)+17134),5,1);
%%g=g/g(end);
%%d_data((1:lp)+17134)=d_data((1:lp)+17134)./g;
%%d_data(1:lp)=mean([d_data(1:lp) d_data(lp+(1:lp))],2)./g;
  d_data(1:lp)=mean([d_data(1:lp) d_data(lp+(1:lp))],2);
  d_data(lp+(1:lp))=d_data(1:lp);
%%g0=conv(ones(45,1),g)./[1:45 45*ones(1,lp-45-1) 45:-1:1]';
%%s1=lp;
%%for l=1:29
%% addr=s1+(1:lp-l); s1=addr(end);
%% d_data(addr)=d_data(addr)./g0(1+l:lp);
%%end
end
