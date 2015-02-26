function dum=t2ps(name_site)
rep=46875; tb=660; % IPP, baudlength (탎) Found in hare.tlan
load hare.ac       % AC-code description file. Look it's commented!
apustr='';
name_site=upper(name_site);

ch_adcint=[5 5]; % Sampling frequency (탎) from hare.elan
ch_filter={'b75d75.fir' 'b75d75.fir'}; % filter file from hare.elan
                                       % also only fir-file in
                                       % hare-info-dir.

ch_f=[499.75 500.25]; % frequencies (MHz), from hare.frq
nsc=size(hare,1); nb=size(hare,2); nf=length(ch_f);
if name_site=='L'
 t=[30 2010]; % RF-on RF-off (탎) from hare.tlan
 n=[46100 46870];% noise injection (탎) CALON CALOFF, from hare.tlan
 s=[2540 46100];% signal sampling (탎) CH1 ALLOFF, from hare.tlan
 c=[46200 46870];% cal sampling (탎) CH2, CH2OFF from hare.tlan
 j=1;
 for i=1:nsc
  i1=i-1;
  p=hare(i,1); tt=i1*rep; ii=rem(i1,nf)+1; iii=rem(i,nf)+1;
  td_t1(j)=t(1)+tt; td_am(j)=p; td_ch(j)=ii;
  for k=2:nb
   if hare(i,k)~=p
    td_t2(j)=t(1)+tt+(k-1)*tb;
    p=hare(i,k); j=j+1;
    td_t1(j)=td_t2(j-1); td_am(j)=p; td_ch(j)=ii;
   end
  end
  td_t2(j)=t(2)+tt; j=j+1;
  td_t1(j)=s(1)+tt; td_am(j)=2; td_ch(j)=ii; td_t2(j)=s(2)+tt; j=j+1;
  td_t1(j)=s(1)+tt; td_am(j)=2; td_ch(j)=iii; td_t2(j)=s(2)+tt; j=j+1;
  % if rem(i1,4)>1 & 0
  td_t1(j)=n(1)+tt; td_am(j)=1; td_ch(j)=0; td_t2(j)=n(2)+tt; j=j+1;
  %  end
  td_t1(j)=c(1)+tt; td_am(j)=2; td_ch(j)=iii; td_t2(j)=c(2)+tt; j=j+1;
 end
end
p_rep=nsc*rep;
p_offsetppd=0;
name_expr='hare';
save_PS
