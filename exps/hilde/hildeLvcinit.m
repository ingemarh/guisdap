% Complete virtual channel definitions
N_scan=64;
T_scan=19920;

on=ones(1,N_scan); sh=ones(4,1)*(0:N_scan-1)*T_scan;
vc_ch=[1;2;3;4]*on;
vc_t1=[0;0;10000;10000]*on+sh;
vc_t2=T_scan+vc_t1;

vc_ch=row(vc_ch);
vc_t1=1+row(vc_t1);
vc_t2=row(vc_t2);
