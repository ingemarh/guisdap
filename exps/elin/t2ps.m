function dum=t2ps(name_site)
rep=5150; tb=200;
load elin.ac
apustr='';
name_site=upper(name_site);
%ch_adcint=[40 40];
%ch_filter={'b12d600.fir' 'b12d600.fir'};
%s=[1000 5000]; c=[5060 5140];
ch_adcint=[50 50];
ch_filter={'b7d750.fir' 'b7d750.fir'};
s=[1050 5000]; c=[5090 5140];
if 1
 apustr=['_2'];
 ch_adcint=[.4 .4];
 ch_filter={'b800d6.fir' 'b800d6.fir'};
 s=[1000 5000]; c=[5070 5140];
end
ch_f=[10 11];
nsc=size(elin,1); nb=size(elin,2); nf=length(ch_f);
if name_site=='V'
 t=[88 688]; n=[5000 5140];
 j=1;
 for i=1:nsc
  i1=i-1;
  p=elin(i,1); tt=i1*rep; ii=rem(i1,nf)+1; iii=rem(i,nf)+1;
  td_t1(j)=t(1)+tt; td_am(j)=p; td_ch(j)=ii;
  for k=2:nb
   if elin(i,k)~=p
    td_t2(j)=t(1)+tt+(k-1)*tb;
    p=elin(i,k); j=j+1;
    td_t1(j)=td_t2(j-1); td_am(j)=p; td_ch(j)=ii;
   end
  end
  td_t2(j)=t(2)+tt; j=j+1;
  td_t1(j)=s(1)+tt; td_am(j)=2; td_ch(j)=ii; td_t2(j)=s(2)+tt; j=j+1;
  if rem(i1,4)>1
   td_t1(j)=n(1)+tt; td_am(j)=1; td_ch(j)=0; td_t2(j)=n(2)+tt; j=j+1;
  end
  td_t1(j)=c(1)+tt; td_am(j)=2; td_ch(j)=iii; td_t2(j)=c(2)+tt; j=j+1;
 end
end
p_rep=nsc*rep;
p_offsetppd=0;
name_expr='elin';
save_PS
