% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='V'
 npp=330; npr1=420; npr2=34596; %pg_bcs(5)='x';
 [ch_el ch_az ch_gain]=vhf_elaz(90,0,10^4.31/2);
else
end
ngat=npp-63;
nsig=npr1+(1:ngat);
d_data(nsig)=zerolagfix(d_data(nsig),d_data(90+(1:npp)),128);
if a_control(4)==1
 d_var1(nsig)=d_var1(nsig)/(1+1/32/63);
 d_var2(nsig)=d_var2(nsig)/(1+1/32/63);
end
nsig=npr2+(1:ngat);
d_data(nsig)=zerolagfix(d_data(nsig),d_data(90+(1:npp)),128);
if a_control(4)==1
 d_var1(nsig)=d_var1(nsig)/(1+1/8/15);
 d_var2(nsig)=d_var2(nsig)/(1+1/8/15);
end
clear npp npr ngat nsig
