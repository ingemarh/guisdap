% Complete virtual channel definitions
N_scan=32*10;

vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=9765;
SHIFT=0;

for scan=0:10:N_scan-1
 for s=1:2
  sub=s+scan;
  vc_ch(sub)=s; vc_t1(sub)=SHIFT; vc_t2(sub)=2*T_scan+SHIFT;
  vc_ch(2+sub)=2+s; vc_t1(2+sub)=0+SHIFT; vc_t2(2+sub)=6000+SHIFT;
  vc_ch(4+sub)=2+s; vc_t1(4+sub)=6000+SHIFT; vc_t2(4+sub)=T_scan+SHIFT;
  vc_ch(6+sub)=2+s; vc_t1(6+sub)=T_scan+SHIFT; vc_t2(6+sub)=2*T_scan+SHIFT;
  vc_ch(8+sub)=4+s; vc_t1(8+sub)=SHIFT; vc_t2(8+sub)=2*T_scan+SHIFT;
  SHIFT=SHIFT+T_scan;
 end
end

