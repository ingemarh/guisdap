% Complete virtual channel definitions
N_scan=32*2*2; 

vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=15600;

for scan=0:2:N_scan-1
  SHIFT=scan/2*T_scan;
  vc_ch(1+scan)=1+rem(scan/2,2);
  vc_t1(1+scan)=0+SHIFT;
  vc_t2(1+scan)=T_scan*2+SHIFT;
  vc_ch(2+scan)=3+rem(scan/2,2);
  vc_t1(2+scan)=0+SHIFT;
  vc_t2(2+scan)=T_scan*2+SHIFT;
end

