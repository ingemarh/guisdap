function dum=t2ps(site)
load(['t_to_ps.txt.' site])
l=size(t_to_ps,1)/2;
td_t1=t_to_ps(1:l,1)';
td_t2=t_to_ps(1:l,2)';
td_am=t_to_ps(1:l,3)';
td_ch=t_to_ps(1:l,4)';
p_rep=640000;
p_offsetppd=0;
ch_adcint=[16 16 16];
ch_filter={'w30d160.fir' 'w30d160.fir' 'w30d160.fir'};
ch_f=[1 2 3];
%for f=1:length(ch_f)
% td_ch(find(td_ch==ch_f(f)))=f;
%end

rt1=rem(td_t1,10000);
hh=find(td_ch==2 & (rt1<600 | rt1>2000));
td_t1=[td_t1 td_t1(hh)];
td_t2=[td_t2 td_t2(hh)];
td_am=[td_am td_am(hh)];
td_ch=[td_ch 4*ones(size(hh))];

rt1=rem(td_t1,10000);
hh=find((td_ch==2 & rt1>2000) | (td_ch==3 & rt1<2000));
td_t1(hh)=[];
td_t2(hh)=[];
td_am(hh)=[];
td_ch(hh)=[];

cal=find(td_ch==3);
td_t1(cal)=td_t1(cal)+2;
d=find(td_t1(cal)>p_rep/4 & td_t1(cal)<p_rep/2);
td_ch(cal(d))=2;
d=find(td_t1(cal)>p_rep/2);
td_ch(cal(d))=1;

d=find(td_ch==4);
td_ch(d)=3;
td_t1(d)=td_t1(d)+1;
save(['steffe' site 'pat_PS'],'td_*','p_*','ch_*')
