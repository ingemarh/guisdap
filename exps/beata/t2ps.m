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
if site=='r'
  d=find(rem(t_to_ps(:,4),1)); t_to_ps(d,:)=[];
end
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if p
 ch_adcint=[.4];
 ch_filter={'b800d6.fir'};
 ch_f=[12];
 site='P';
 p_rep=357120;
 apustr='_2';
elseif site=='l'
 ch_adcint=[25 25];
 ch_filter={'b14d375.fir' 'b14d375.fir'};
 ch_f=[500.3 500.5];
 p_rep=400000;
elseif site=='t'
 ch_adcint=[10 10];
 ch_filter={'b35d150.fir' 'b35d150.fir'};
 ch_f=[12 12.1];
 p_rep=357120;
elseif site=='r'
 ch_adcint=[10];
 ch_filter={'b35d150.fir'};
 ch_f=[12];
 p_rep=357120;
 if rc==2
td_ch=t_to_ps(:,4)';
  d=find(td_ch==4); td_ch(d)=12;
  ch_adcint=[20];
  ch_filter={'b25d300.fir'};
 end
elseif site=='v'
 ch_adcint=[20 20];
 %ch_filter={'b18d300.fir' 'b18d300.fir'}; % until 11Apr13
 ch_filter={'b25d300.fir' 'b25d300.fir'};
 ch_f=[12 12.2];
 p_rep=357120;
else
 error('giveup')
end
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='beata';
name_site=upper(site);
save_PS
