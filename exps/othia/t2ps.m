function dum=t2ps(site,rc,p)
if nargin<2, rc=0; end
if nargin<3, p=0; end
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if site=='l'
 ch_adcint=[6 6 6];
 ch_filter={'b58d90.fir' 'b58d90.fir' 'b58d90.fir'};
 ch_f=[500.2 500.35 500.5];
 if rc==1, ch_f=[-1 0 1]+499.75, end
 p_rep=90000;
elseif site=='t'
 ch_adcint=[4 4 4];
 ch_filter={'b85d60.fir' 'b85d60.fir' 'b85d60.fir'};
 ch_f=[5 10 14];
 p_rep=96000;
elseif site=='r'
 ch_adcint=[12 12];
 ch_filter={'b25d225.fir' 'b25d225.fir'};
 ch_f=[5 6];
 p_rep=144000;
 p_offsetppd=-3000;
elseif site=='v'
 ch_adcint=[12 12 12];
 ch_filter={'b29d180.fir' 'b29d180.fir' 'b29d180.fir'};
 ch_f=[5 6 10];
 p_rep=144000;
else
 error('giveup')
end
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='othia';
name_site=upper(site);
save_PS
