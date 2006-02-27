% GUISDAP v1.50   94-03-10 Copyright Asko Huuskonen, Markku Lehtinen 
% 
                                                                     
% ALTCP1Rvcinit.m
% Complete virtual channel definitions
% 128 or 256 virtual channels will be formed
 N_scan=32; % defines only half of the cycle 
% N_scan=64; % defines the whole cycle (needs 128 Mb memory!)
vc_ch=zeros(1,4*N_scan);
vc_t1=zeros(1,4*N_scan);
vc_t2=zeros(1,4*N_scan);
T_scan=15302;

% One round of the main loops defines 4 virtual channels for one scan,
% which lasts for 15302 us.

for scan=0:N_scan-1
  SHIFT=scan*T_scan;
%                       long    altern 
%                       pulse    code  
  vc_ch((1:4)+4*scan)=[ 3  4    5   6  ];
  vc_t1((1:4)+4*scan)=[ 0  0   0.2 0.7 ]*T_scan+SHIFT;
  vc_t2((1:4)+4*scan)=[ 1  1   1.0 1.5 ]*T_scan+SHIFT;
end

for scan=32:N_scan-1 % This loop is not executed, if N_scan=32
  SHIFT=scan*T_scan;
% Channels 7,8 receive alternating codes during the latter half
%                       long    altern 
%                       pulse    code  
  vc_ch((1:4)+4*scan)=[ 3  4    7   8  ];
  vc_t1((1:4)+4*scan)=[ 0  0   0.2 0.7 ]*T_scan+SHIFT;
  vc_t2((1:4)+4*scan)=[ 1  1   1.0 1.5 ]*T_scan+SHIFT;
end
ind=find(vc_t2>64*T_scan);
vc_t2(ind)=vc_t2(ind)-64*T_scan;
