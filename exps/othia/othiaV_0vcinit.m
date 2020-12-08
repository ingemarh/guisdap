% Complete virtual channel definitions
N_scan=12;

%N_scan=1; 
vc_ch=zeros(N_scan,3);
vc_t1=zeros(N_scan,3);
vc_t2=zeros(N_scan,3);
T_scan=12000;

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
  vc_ch(1+scan,:)=1:3;
  vc_t1(1+scan,:)=0+SHIFT;
  vc_t2(1+scan,:)=T_scan+SHIFT;
end
vc_ch=reshape(vc_ch',1,N_scan*3);
vc_t1=reshape(vc_t1',1,N_scan*3);
vc_t2=reshape(vc_t2',1,N_scan*3);
