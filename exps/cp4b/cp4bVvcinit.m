% Complete virtual channel definitions
N_scan=4;
vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=14000;
for scan=1:N_scan
  vc_ch(scan)=scan;
  vc_t1(scan)=0;
  vc_t2(scan)=T_scan;
end

