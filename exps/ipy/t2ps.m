function dum=t2ps(site,rc,p)
if nargin<2, rc=0; end
if nargin<3, p=0; end
t2psfile='t_to_ps.txt';
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' lower(site)];
if p & rc<4
 t2psfile=['t_to_ps.txt_1.' lower(site)];
end
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if p
 if rc<3
  ch_adcint=[.6];
  ch_filter={'w600d6.fir'};
 else
  ch_adcint=[.4];
  ch_filter={'b800d6.fir'};
 end
 ch_f=[499.85];
 sig=find(td_ch==ch_f & td_am==2 & rem(td_ch,3750)<1500);
 td_t1(sig)=td_t1(sig)+.2;
 sig=find(td_ch==ch_f & abs(td_am)==1);
 td_t1(sig)=td_t1(sig)-.2;
 td_t2(sig)=td_t2(sig)-.2;
 site='P';
elseif site=='l'
 ch_adcint=[15 15];
 ch_filter={'b30d225.fir' 'b30d225.fir'};
 ch_f=[499.85 499.95];
 if rc>3
  ch_f(2)=500.15;
  ch_filter={'b25d225.fir' 'b25d225.fir'};
 end
else
 error('giveup')
end
p_rep=240000;
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='ipy';
name_site=upper(site);
save_PS
