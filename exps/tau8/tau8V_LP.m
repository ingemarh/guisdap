N_SCAN=length(vc_ch);

COR_init(977*N_SCAN,80)
for vc=1:N_SCAN;
    COR_uprog(0,1,vc,'s',1,600,(0:5)*14,0,1)
    COR_fraclp(3585,vc,'s',510,16,84,(1:47)*14,1)
%   COR_pp(50712,1,vc,'c',1,21,0,1)
    COR_uprog(50733,1,vc,'b',1,600,(0:5)*14,0,1)
    COR_pp(54318,1,vc,'c',1,21,0,1)
end
p_rep=p_rep*N_SCAN/64;
   
COR_end
