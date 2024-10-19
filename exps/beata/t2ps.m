function dum=t2ps(site,rc,p)
if nargin<2, rc=0; end
if nargin<3, p=0; end
[t2psfile,apu]=find_apustr_file('t_to_ps.txt',rc,'',lower(site));
t2psfile=[t2psfile '.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
if site=='r'
  d=find(rem(t_to_ps(:,4),1)); t_to_ps(d,:)=[];
end
p_offsetppd=0;
if p
 ch_adcint=[.4];
 ch_filter={'b800d6.fir'};
 if site=='T'
  ch_f=[12];
  p_rep=357120;
  apustr='_2';
 elseif site=='L'
  ch_f=[500.3];
  p_rep=400000;
  apustr='';
 end
 site='P';
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
 if rc==2
  ch_adcint=[10 10 0.4];
  ch_f=[8 8.1 99];
  ch_filter={'b35d150.fir' 'b35d150.fir' 'b800d6.fir'};
 elseif rc==2.3
  ch_adcint=[10 10 4/15];
  ch_f=[8 8.1 99];
  ch_filter={'b35d150.fir' 'b35d150.fir' 'b1230d4.fir'};
 end
elseif site=='r'
 ch_adcint=[10];
 ch_filter={'b35d150.fir'};
 ch_f=[12];
 p_rep=357120;
 if rc==2
  d=find(t_to_ps(:,4)==4); t_to_ps(d,4)=12;
  ch_adcint=[20];
  ch_filter={'b25d300.fir'};
 end
elseif site=='v'
 ch_adcint=[20 20];
 %ch_filter={'b18d300.fir' 'b18d300.fir'}; % until 11Apr13
 ch_filter={'b25d300.fir' 'b25d300.fir'};
 ch_f=[12 12.2];
 p_rep=357120;
 if rc==3
  ch_adcint=[10 10];
  ch_f=[4 4.2];
  ch_filter={'b35d150.fir' 'b35d150.fir'};
 end
else
 error('giveup')
end
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='beata';
name_site=upper(site);
if apu, apustr=['_' num2str(rc)]; end
save_PS
