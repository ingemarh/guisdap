N_SCAN=length(vc_ch);
COR_init(637*N_SCAN,43)
for vc=1:N_SCAN
    COR_fraclp(465,vc,'s',95-5*15,16,60,(1:29)*12,1)
    COR_uprog(0,1,vc,'s',1,95,(0:4)*12,0,1)
    COR_pp(4335,1,vc,'c',1,15,0,1)
    COR_uprog(4350,1,vc,'b',1,256,(0:4)*12,0,1)
    COR_pp(5620,1,vc,'c',1,15,0,1)
end
   
COR_end
