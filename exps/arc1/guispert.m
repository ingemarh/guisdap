% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='T'
 npp=426; nslic=9; prep=443904;
 lpg_bcs(1:nslic)='x';
elseif name_site=='L'
 npp=505; nslic=10; prep=1009920/2;
 lpg_bcs(1:nslic)='x';
else
 npp=94; nslic=1; prep=0;
 ch_height=292.9;
end
if exist('analysis_code','var')
 d_time=toYMDHMS(d_time(1),tosecs(d_time)+[min(analysis_code)-1;max(analysis_code)-nslic]*prep/1e6);
end
ngat=npp-63;
npr=ngat*16;
for slic=0:nslic-1
 nsig=nslic*npp+slic*npr+(1:ngat);
 d_data(nsig)=remove_stripe(d_data(nsig),d_data(slic*npp+(1:npp)),128);
end
nsig=col((1:ngat)'*ones(1,nslic)+ones(ngat,1)*(0:nslic-1)*npr)+nslic*npp;
if a_control(4)==1
 d_var1(nsig)=d_var1(nsig)/(1+1/6);
 d_var2(nsig)=d_var2(nsig)/(1+1/6);
end
clear npp npr nslic ngat slic prep nsig
