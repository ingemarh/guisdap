COR_init
addsh=0; vc=1;
for ac=1:2
    COR_uprog(7619+addsh,1,vc,'s',1,638,(0:29)*15,0,ac)
    vc=vc+1;
    COR_pp(0+addsh,1,vc,'s',1,166,0,ac)
    COR_uprog(166+addsh,1,vc,'b',1,261,(0:29)*15,0,ac)
    COR_pp(7561+addsh,1,vc,'c',1,58,0,ac)
    addsh=26324; vc=vc+1;
end
   
COR_end
