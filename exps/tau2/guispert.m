% guispert.m: special experiment specific hacks
% GUISDAP v1.81   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
global p_rep a_control v_lightspeed
p_rep=180000;
ch_Pt=ch_Pt(1);
if name_site=='T'
  d_data(1:364)=d_data(1:364)+d_data(364+(1:364));
  if a_control(4)==1
    d_var1(1:364)=sqrt(2)*d_var1(1:364);
    d_var2(1:364)=sqrt(2)*d_var2(1:364);
  end
else
  ch_height=292.9;
end
