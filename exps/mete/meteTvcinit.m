% GUISDAP v8.5   07-04-10
% 
N_scan=16;
vc_ch=zeros(1,6*N_scan);
vc_t1=zeros(1,6*N_scan);
vc_t2=zeros(1,6*N_scan);
vc_mf=zeros(1,6*N_scan);
T_scan=5330;
p=2113/T_scan;

for scan=0:N_scan-1
  SHIFT=scan*T_scan*2;
%                      pulse profile code
  vc_ch((1:6)+6*scan)=[1  2   3  4   3 4 ];
  vc_t1((1:6)+6*scan)=[0  1   0  1   p 1+p]*T_scan+SHIFT;
  vc_t2((1:6)+6*scan)=[2  3   p 1+p  2 3 ]*T_scan+SHIFT;
  vc_mf((1:6)+6*scan)=[0  0  13  13  13 13];
end
vc_t2([2 6]+6*scan)=4*T_scan+SHIFT;
