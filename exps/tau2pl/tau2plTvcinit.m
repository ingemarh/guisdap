% Complete virtual channel definitions
 N_scan=64; % Do only half

%N_scan=1; 
vc_ch=zeros(1,N_scan*2);
vc_t1=zeros(1,N_scan*2);
vc_t2=zeros(1,N_scan*2);
T_scan=5580;

for scan=0:N_scan-1
  SHIFT=scan*T_scan; %remove the 2 for 64 scans
  vc_ch(1+scan)=1+rem(scan,2);
  vc_t1(1+scan)=0+SHIFT;
  vc_t2(1+scan)=T_scan*2+SHIFT;
  vc_ch(65+scan)=3;
  vc_t1(65+scan)=0+SHIFT;
  vc_t2(65+scan)=T_scan+SHIFT;
end

