% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
p_rep=180000*2;
ch_Pt=ch_Pt(1);
if name_site=='K' | name_site=='S'
  ch_height=292.9;
elseif name_site=='T'
  d_data(1:728)=d_data(1:728)+d_data(728+(1:728));
% g=gainfit(d_data(34956:35683));
% d_data(34956:35683)=d_data(34956:35683)./g;
% d_data(1:728)=d_data(1:728)./g;
  if a_control(4)==1
    d_var1(1:728)=sqrt(2)*d_var1(1:728);
    d_var2(1:728)=sqrt(2)*d_var2(1:728);
  end	    
end
