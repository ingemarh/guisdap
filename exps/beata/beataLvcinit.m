% Complete virtual channel definitions
N_scan=64;

%N_scan=1; 
vc_ch=zeros(1,N_scan*2);
vc_t1=zeros(1,N_scan*2);
vc_t2=zeros(1,N_scan*2);
T_scan=6250;

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  vc_ch(1+scan)=1;
  vc_t1(1+scan)=0+SHIFT;
  vc_t2(1+scan)=T_scan+SHIFT;
  vc_ch(N_scan+1+scan)=2;
  vc_t1(N_scan+1+scan)=0+SHIFT;
  vc_t2(N_scan+1+scan)=T_scan+SHIFT;
end

