function dum=t2ps(name_site)
rep=10000; tb=60;
%load pia.ac
load tau0.ac, pia=tau0;
ch_adcint=[20 20 20 20];
ch_filter={'w25d200.fir' 'w25d200.fir' 'w25d200.fir' 'w25d200.fir'};
ch_f=[4 5 6 7];
nsc=size(pia,1); nb=size(pia,2);
t=[50 1010;1070 2030]; s=[2364 9684]; c=[9725 9965]; n=s; ni=[9685 9966.1];
j=1;
for i=1:2:nsc
 i1=i-1;
 for m=1:2
  cod=i1+m; m1=m-1;
  tt=(i1+(m-1))*rep; ii=m1*2+1; iii=rem(m*2+1,4);
  for l=1:2
   l1=l-1;
   p=pia(cod,1);
   td_t1(j)=t(l,1)+tt; td_am(j)=p; td_ch(j)=ii+l1;
   for k=2:nb
    if pia(cod,k)~=p
     td_t2(j)=t(l,1)+tt+(k-1)*tb; j=j+1;
     p=pia(cod,k);
     td_t1(j)=td_t2(j-1); td_am(j)=p; td_ch(j)=ii+l1;
    end
   end
   td_t2(j)=t(l,2)+tt; j=j+1;
  end
  for l1=0:1
   td_t1(j)=s(1)+tt; td_am(j)=2; td_ch(j)=ii+l1; td_t2(j)=s(2)+tt; j=j+1;
   td_t1(j)=n(1)+tt; td_am(j)=2; td_ch(j)=iii+l1; td_t2(j)=n(2)+tt; j=j+1;
  end
  td_t1(j)=ni(1)+tt; td_am(j)=1; td_ch(j)=0; td_t2(j)=ni(2)+tt; j=j+1;
  for l1=0:1
   td_t1(j)=c(1)+tt; td_am(j)=2; td_ch(j)=ii+l1; td_t2(j)=c(2)+tt; j=j+1;
   td_t1(j)=c(1)+tt; td_am(j)=2; td_ch(j)=iii+l1; td_t2(j)=c(2)+tt; j=j+1;
  end
 end
end
p_rep=2*nsc*rep;
p_offsetppd=0;
eval(['save tau0' name_site 'pat_PS ch_* p_* td_*'])
