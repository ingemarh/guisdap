function dum=t2ps(name_site)
rep=2890; tb=100;
load pia.ac
ch_adcint=[10 10];
ch_filter={'b35d150.fir' 'b35d150.fir'};
ch_f=[14 15];
nsc=size(pia,1); nb=size(pia,2); nf=length(ch_f);
if name_site=='T'
 t=[88 388]; s=[700 2760]; c=[2795 2875]; n=[2760 2875];
 j=1;
 for i=1:nsc
  i1=i-1;
  p=pia(i,1); tt=i1*rep; ii=rem(i1,nf)+1; iii=rem(i,nf)+1;
  td_t1(j)=t(1)+tt; td_am(j)=p; td_ch(j)=ii;
  for k=2:nb
   if pia(i,k)~=p
    td_t2(j)=t(1)+tt+(k-1)*tb;
    p=pia(i,k); j=j+1;
    td_t1(j)=td_t2(j-1); td_am(j)=p; td_ch(j)=ii;
   end
  end
  td_t2(j)=t(2)+tt; j=j+1;
  td_t1(j)=s(1)+tt; td_am(j)=2; td_ch(j)=ii; td_t2(j)=s(2)+tt; j=j+1;
  if i>4
   td_t1(j)=n(1)+tt; td_am(j)=1; td_ch(j)=0; td_t2(j)=n(2)+tt; j=j+1;
  end
  td_t1(j)=c(1)+tt; td_am(j)=2; td_ch(j)=iii; td_t2(j)=c(2)+tt; j=j+1;
 end
end
p_rep=nsc*rep;
p_offsetppd=0;
eval(['save pia' name_site 'pat_PS ch_* p_* td_*'])
