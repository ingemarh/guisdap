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
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
if rc==2
 td_t1=t_to_ps(:,1)';
 td_t2=t_to_ps(:,2)';
 td_am=t_to_ps(:,3)';
 td_ch=t_to_ps(:,4)';
 p_rep=300000;
 if p
  ch_adcint=[.6];
  ch_filter={'w600d6.fir'};
  ch_f=[1];
  sig=find(td_ch==1 & td_am==2);
  td_t1(sig)=td_t1(sig)+100;
  site='P';
 else
  ch_adcint=[15 15];
  ch_filter={'b30d225.fir' 'b30d225.fir'};
  ch_f=[1 2];
 end

 cal=find(td_ch==3);
 td_t1(cal)=td_t1(cal)+2;
 td_ch(cal)=1;
else
 l=size(t_to_ps,1)/2;
 td_t1=t_to_ps(1:l,1)';
 td_t2=t_to_ps(1:l,2)';
 td_am=t_to_ps(1:l,3)';
 td_ch=t_to_ps(1:l,4)';
 p_rep=640000;
 ch_adcint=[16 16 16];
 ch_filter={'w30d160.fir' 'w30d160.fir' 'w30d160.fir'};
 ch_f=[1 2 3];

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
end

name_expr='steffe';
name_site=upper(site);
save_PS
