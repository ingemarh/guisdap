% Complete virtual channel definitions
N_scan=32;

%N_scan=1; 
vc_ch=zeros(1,N_scan*6);
vc_t1=zeros(1,N_scan*6);
vc_t2=zeros(1,N_scan*6);
T_scan=20000;
T1=6425; T2=13200;

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  vc_ch(1+scan*6)=1;
  vc_t1(1+scan*6)=0+SHIFT;
  vc_t2(1+scan*6)=T1+SHIFT;
  vc_ch(2+scan*6)=2;
  vc_t1(2+scan*6)=0+SHIFT;
  vc_t2(2+scan*6)=T1+SHIFT;
  vc_ch(3+scan*6)=1;
  vc_t1(3+scan*6)=T1+SHIFT;
  vc_t2(3+scan*6)=T_scan+SHIFT;
  vc_ch(4+scan*6)=2;
  vc_t1(4+scan*6)=T1+SHIFT;
  vc_t2(4+scan*6)=T_scan+SHIFT;
  vc_ch(5+scan*6)=3;
  vc_t1(5+scan*6)=0+SHIFT;
  vc_t2(5+scan*6)=T_scan+SHIFT;
  vc_ch(6+scan*6)=4;
  vc_t1(6+scan*6)=0+SHIFT;
  vc_t2(6+scan*6)=T_scan+SHIFT;
end
