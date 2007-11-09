% Complete virtual channel definitions
N_scan=32;

vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=2*11158;

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  vc_ch(1+scan)=1;
  vc_t1(1+scan)=0+SHIFT;
  vc_t2(1+scan)=T_scan+SHIFT;
end
