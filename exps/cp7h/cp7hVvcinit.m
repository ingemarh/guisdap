% Complete virtual channel definitions
N_scan=4; %skip other half 
vc_ch=zeros(1,N_scan*3);
vc_t1=zeros(1,N_scan*3);
vc_t2=zeros(1,N_scan*3);
T_scan=18465;
for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  vc_ch((1:3)+scan*3)=rem([2 3 4]-1-scan+4,4)+1;
  vc_t1((1:3)+scan*3)=[0 0 0]+SHIFT;
  vc_t2((1:3)+scan*3)=[1 1 2]*T_scan+SHIFT;
end

