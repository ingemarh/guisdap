% Complete virtual channel definitions
N_scan=32; 

vc_ch=zeros(1,N_scan*2);
vc_t1=zeros(1,N_scan*2);
vc_t2=zeros(1,N_scan*2);
T_scan=10000;

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  ch=rem(scan,2)*2+1;
  vc_ch(1+scan*2)=ch;
  vc_t1(1+scan*2)=0+SHIFT;
  vc_t2(1+scan*2)=T_scan*2+SHIFT;
  vc_ch(2+scan*2)=ch+1;
  vc_t1(2+scan*2)=0+SHIFT;
  vc_t2(2+scan*2)=T_scan*2+SHIFT;
end

