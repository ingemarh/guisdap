function dum=t2ps(site,rc)
global name_expr
if nargin<2, rc=0; end
t2psfile='t_to_ps.txt';
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=240000;
name_site=upper(site);
if name_site=='L'
 ch_f=500;
 if rc==3 | rc==4
  ch_adcint=2;
  ch_filter={'b170d30.fir'};
 else
  ch_adcint=5;
  ch_filter={'b100d75.fir'};
 end
 if rc>1
  p_rep=160000;
 end
elseif name_site=='V'
 if rc==4
  d=find(rem(td_t1,1500)<1000);
  td_t1(d)=td_t1(d)-1; td_t2(d)=td_t2(d)-1;
  ch_f=10;
  ch_adcint=1.2;
  ch_filter={'b300d18.fir'};
  p_rep=192000;
 elseif rc==3
  ch_f=10;
  ch_adcint=4;
  ch_filter={'b125d60.fir'};
  p_rep=320000;
 else
  ch_f=9;
  ch_adcint=3;
  ch_filter={'b170d45.fir'};
 end
else
 ch_f=12;
 if rc==4
  d=find(rem(td_t1,1500)<1000);
  td_t1(d)=td_t1(d)-1; td_t2(d)=td_t2(d)-1;
  if name_site=='T'
   ch_adcint=1.2;
   ch_filter={'b300d18.fir'};
  else
   ch_adcint=2.4;
   ch_filter={'b210d36.fir'};
  end
  p_rep=192000;
 elseif rc==3
  ch_adcint=2;
  ch_filter={'b170d30.fir'};
  %ch_filter={'b250d30.fir'};
  p_rep=320000;
 else
  ch_adcint=3;
  ch_filter={'b170d45.fir'};
 end
end
p_offsetppd=0;
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
name_expr='manda';
save_PS
