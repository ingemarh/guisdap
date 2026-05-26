function dum=t2ps(site,rc,p)
if nargin<2, rc=0; end
if nargin<3, p=0; end
[t2psfile,apu]=find_apustr_file('t_to_ps.txt',rc,'',lower(site));
t2psfile=['t_to_ps.txt_1.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if p
 ch_adcint=[1/3];
 ch_filter={'b800d5.fir'};
 ch_f=[499.75];
 sig=find(td_ch==ch_f & td_am==2 & rem(td_ch,6250)<2500);
 td_t1(sig)=td_t1(sig)+.2;
 sig=find(td_ch==ch_f & abs(td_am)==1);
 td_t1(sig)=td_t1(sig)-.2;
 td_t2(sig)=td_t2(sig)-.2;
 site='P';
elseif site=='l'
 ch_adcint=[25 25]/3;
 ch_filter={'b45d125.fir' 'b45d125.fir'};
 ch_f=[499.75 500.25];
else
 error('giveup')
end
p_rep=800000;
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='mio';
name_site=upper(site);
if apu, apustr=['_' num2str(rc)]; end
save_PS
