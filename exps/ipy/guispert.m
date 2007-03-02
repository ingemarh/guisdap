% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
d_date=datenum(d_time(1,:));
if length(d_data)>10063
  calTemp=[163 163 228 228];
  if length(lpg_ND)<1887
    nsh=length(lpg_ND);
    nsh2=length(lp_vc);
    nsh3=length(d_data)/2;
    ch_fradar=repmat(ch_fradar,1,2);
    lp_vc=[lp_vc lp_vc+128];
    lpg_ND=repmat(lpg_ND,1,2);
    lpg_T=repmat(lpg_T,1,2);
    d=find(lpg_bac);
    lpg_bac=[lpg_bac zeros(1,nsh)]; lpg_bac(d+nsh)=lpg_bac(d)+nsh;
    lpg_bcs=repmat(lpg_bcs,1,2);
    lpg_cal=[lpg_cal lpg_cal+nsh];
    lpg_code=[lpg_code lpg_code+2];
    lpg_dt=repmat(lpg_dt,1,2);
    lpg_h=repmat(lpg_h,1,2);
    lpg_lag=repmat(lpg_lag,1,2);
    lpg_lpdata=[lpg_lpdata lpg_lpdata+nsh2];
    lpg_lpend=[lpg_lpend lpg_lpend+nsh2];
    lpg_lpstart=[lpg_lpstart lpg_lpstart+nsh2];
    lpg_nt=repmat(lpg_nt,1,2);
    lpg_ra=[lpg_ra lpg_ra+nsh3];
    lpg_ri=repmat(lpg_ri,1,2);
    lpg_w=repmat(lpg_w,1,2);
    lpg_womscaled=repmat(lpg_womscaled,2,1);
    lpg_wr=repmat(lpg_wr,1,2);
    vc_ch=[vc_ch vc_ch+2];
    vc_group=[vc_group vc_group+128];
    vc_routine=repmat(vc_routine,1,2);
    vc_Ap=repmat(vc_Ap,1,2);
    ch_Pt=repmat(ch_Pt,1,2);
    form_adpar
    global lpg_Ap
    lpg_Ap=repmat(lpg_Ap,2,1);
    if isempty(a_code)
      a_satch.clutter=repmat(a_satch.clutter,1,2);
      a_satch.repair=repmat(a_satch.repair,1,2);
    end

    ch_gain=[ch_gain ch_gain*0.64];
  end
end
ng=length(ch_gain)/2;
glp=1886;
grps=[1 1 lpg_h(1);2 1884 lpg_h(1)+lpg_w(1)/2
      1885 1886 lpg_h(1886)];
for i=1:ng
  gaincorrect(glp,grps)
  glp=glp+1886; grps(:,1:2)=grps(:,1:2)+1886;
end
