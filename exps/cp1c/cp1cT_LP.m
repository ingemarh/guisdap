COR_init

lags=[3 4 9 11 1 6 8 5 7 2]; nmp=12*30;
for vc=1:2
 COR_mp((vc-1)*nmp+2,12,vc,'s',1,24,lags*30,0,vc)
 for i=[0 lags(1:4)*2]
  COR_pp((vc-1)*nmp+1   ,12,vc,'x',1,24,i,vc)
 end
 COR_mp((vc-1)*nmp+2+24*12,12,vc,'c',1, 3,lags*30,0,vc)
 for i=[0 lags(1:4)*2]
  COR_pp((vc-1)*nmp+1+24*12,12,vc,'c',1,3,i,vc)
 end
 COR_mp((vc-1)*nmp+2+27*12,12,vc,'b',1, 3,lags*30,0,vc)
 for i=[0 lags(1:4)*2]
  COR_pp((vc-1)*nmp+1+27*12,12,vc,'b',1,3,i,vc)
 end
end

vc=3;
COR_pp(nmp*2   ,1,vc,'s',1, 40,0,3)
COR_pp(nmp*2+40,1,vc,'c',1, 10,0,3)
COR_pp(nmp*2+50,1,vc,'b',1, 10,0,3)
vc=4;
COR_pp(nmp*2+ 60,1,vc,'s',1,174,0,4)
COR_pp(nmp*2+234,1,vc,'c',1, 18,0,4)
COR_pp(nmp*2+252,1,vc,'b',1, 18,0,4)

vc=5;
COR_trilp(1+nmp*2+270      ,33,vc,'s',36,-18,20,0:10:310,5)
COR_trilp(1+nmp*2+270+33*21,33,vc,'c',36,-18, 5,0:10:310,5)
COR_trilp(1+nmp*2+270+33*27,33,vc,'b',36,-18, 5,0:10:310,5)

COR_end
