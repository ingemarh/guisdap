% Complete virtual channel definitions
N_scan=32*6;

vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=9765;
SHIFT=0;

for scan=0:6:N_scan-1
 for s=1:2
  sub=s+scan;
  vc_ch(sub)=s; vc_t1(sub)=SHIFT; vc_t2(sub)=2*T_scan+SHIFT;
% vc_ch(2+sub)=s; vc_t1(2+sub)=T_scan+SHIFT; vc_t2(2+sub)=2*T_scan+SHIFT;
  vc_ch(2+sub)=2+s; vc_t1(2+sub)=6000+SHIFT; vc_t2(2+sub)=T_scan+SHIFT;
  vc_ch(4+sub)=2+s; vc_t1(4+sub)=T_scan+SHIFT; vc_t2(4+sub)=2*T_scan+SHIFT;
  SHIFT=SHIFT+T_scan;
 end
end

