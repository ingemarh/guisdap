% Complete virtual channel definitions
 N_scan=64; %skip other half 

%N_scan=1; 
vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=15624;

for scan=0:2:N_scan-1
  SHIFT=scan*T_scan; %remove the 2 for 64 scans
  vc_ch(1+scan)=1;
  vc_t1(1+scan)=0+SHIFT;
  vc_t2(1+scan)=T_scan*2+SHIFT;
  vc_ch(2+scan)=2;
  vc_t1(2+scan)=T_scan+SHIFT;
  vc_t2(2+scan)=T_scan*3+SHIFT;
end

