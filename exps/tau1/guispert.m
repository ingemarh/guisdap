% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if length(d_data)<26700
 if exist('vhf_half_tx','var')
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el,0,10^4.31/2),
 end
 analysis_code=1;
else
 if length(analysis_code)==1
  polhv=[.6374 -45.17 540.33;.6336 -48.418 790.23;0 0 -ch_Pt(1)/1000];
  hv=max(roots(sum(polhv)));
  ch_Pt=polyval(polhv(analysis_code,:),hv)*1000;
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el(analysis_code),12*(analysis_code-1),10^4.31/2);
 else
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el,0,10^4.31/2);
 end
end
if name_site=='T'
  lp=728;
  d_data(1:lp)=mean([d_data(1:lp) d_data(lp+(1:lp))],2);
  d_var1(1:lp)=mean([d_var1(1:lp) d_var1(lp+(1:lp))],2);
  d_var2(1:lp)=mean([d_var2(1:lp) d_var2(lp+(1:lp))],2);
  d_data(lp+(1:lp))=d_data(1:lp);
  d_var1(lp+(1:lp))=d_var1(1:lp);
  d_var2(lp+(1:lp))=d_var2(1:lp);
elseif name_site=='V'
  if any(analysis_code==1)
    lp=546;
    d_data(1:lp)=mean([d_data(1:lp) d_data(lp+(1:lp))],2);
    d_var1(1:lp)=mean([d_var1(1:lp) d_var1(lp+(1:lp))],2);
    d_var2(1:lp)=mean([d_var2(1:lp) d_var2(lp+(1:lp))],2);
    d_data(lp+(1:lp))=d_data(1:lp);
    d_var1(lp+(1:lp))=d_var1(1:lp);
    d_var2(lp+(1:lp))=d_var2(1:lp);
  end
  if any(analysis_code==2)
    lp=546; shft=26610;
    d_data(shft+(1:lp))=mean([d_data(shft+(1:lp)) d_data(shft+lp+(1:lp))],2);
    d_var1(shft+(1:lp))=mean([d_var1(shft+(1:lp)) d_var1(shft+lp+(1:lp))],2);
    d_var2(shft+(1:lp))=mean([d_var2(shft+(1:lp)) d_var2(shft+lp+(1:lp))],2);
    d_data(shft+lp+(1:lp))=d_data(shft+(1:lp));
    d_var1(shft+lp+(1:lp))=d_var1(shft+(1:lp));
    d_var2(shft+lp+(1:lp))=d_var2(shft+(1:lp));
  end
end
