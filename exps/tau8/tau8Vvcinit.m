% Complete virtual channel definitions
skip=2; %skip 2nd ch
N_scan=64/skip;

%N_scan=1; 
vc_ch=zeros(1,N_scan);
vc_t1=zeros(1,N_scan);
vc_t2=zeros(1,N_scan);
T_scan=13020;

for scan=0:N_scan-1
  SHIFT=scan*T_scan*skip;
  vc_ch(1+scan)=rem(scan,3-skip)+1;
  vc_t1(1+scan)=0+SHIFT;
  vc_t2(1+scan)=2*T_scan+SHIFT;
end

