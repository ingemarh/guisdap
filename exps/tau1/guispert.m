% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
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
