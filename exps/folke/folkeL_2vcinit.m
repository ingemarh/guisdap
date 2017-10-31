% Complete virtual channel definitions
N_scan=32;

%N_scan=1; 
vc_ch=zeros(1,N_scan*3);
vc_t1=zeros(1,N_scan*3);
vc_t2=zeros(1,N_scan*3);
T_scan=20000;
T1=7975;

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  vc_ch(1+scan*3)=1;
  vc_t1(1+scan*3)=0+SHIFT;
  vc_t2(1+scan*3)=T1+SHIFT;
  vc_ch(2+scan*3)=1;
  vc_t1(2+scan*3)=T1+SHIFT;
  vc_t2(2+scan*3)=T_scan+SHIFT;
  vc_ch(3+scan*3)=2;
  vc_t1(3+scan*3)=T1+SHIFT;
  vc_t2(3+scan*3)=T_scan+SHIFT;
end
