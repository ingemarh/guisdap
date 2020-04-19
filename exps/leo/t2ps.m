function dum=t2ps(name_site)
version='2';
ac=44-char(textread('ac64.txt','%s'));
[nsc,nb]=size(ac);
rep=20000; tb=30; frac=2;
ch_filter=tb/frac; %boxcar
ch_adcint=1*ch_filter;
ch_f=12;
offset=0;
lowtail=48;
if name_site=='T'
 t=82+[0 tb*nb]; s=[3+ceil(t(2)/ch_filter) floor((19997-100)/ch_filter)]; c=[(s(2)+1)*ch_filter 19997];
 ns=diff(s)+1;
 par=[[24 128 ns/frac-nb+1+lowtail 0 nsc nsc*5 ns nb frac 1 ...
	0 lowtail 0 0 0 1 64 0 0 0 ...
	0 1 145 1]';col(ac')];
 find(rem(par,1))
 fid=fopen('leo_u.par','w'); fprintf(fid,'%d\n',par); fclose(fid);
 fprintf('Samps s1=%.0f ntx=%d ns=%d nc=%d\n',s(1)*ch_filter+2,s(1)-1,diff(s)+1,diff(c)+1)
 j=1;
 s=s*ch_filter;
 for i=1:nsc
  i1=i-1;
  p=ac(i,1); tt=i1*rep;
  toff=t+rem(i-1,8)*offset;
  td_t1(j)=t(1)+tt; td_am(j)=p; td_ch(j)=1;
  for k=2:nb
   if ac(i,k)~=p
    td_t2(j)=t(1)+tt+(k-1)*tb;
    p=ac(i,k); j=j+1;
    td_t1(j)=td_t2(j-1); td_am(j)=p; td_ch(j)=1;
   end
  end
  td_t2(j)=t(2)+tt; j=j+1;
  td_t1(j)=s(1)+tt; td_am(j)=2; td_ch(j)=1; td_t2(j)=s(2)+tt; j=j+1;
  if rem(i,2)
   td_t1(j)=c(1)+tt-1; td_am(j)=1; td_ch(j)=0; td_t2(j)=c(2)+tt; j=j+1;
  end
  td_t1(j)=c(1)+tt; td_am(j)=2; td_ch(j)=1; td_t2(j)=c(2)+tt; j=j+1;
 end
end
p_rep=nsc*rep;
p_offsetppd=0;
eval(['save leo' name_site 'pat_PS ch_* p_* td_*'])
