% guispert.m: special experiment specific hacks
% GUISDAP v1.70   01-11-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
ch_Pt=ch_Pt(1);
if name_site=='T'
  lpg_bcs(1)='x';
  d_data(439+(1:376))=remove_stripe(d_data(439+(1:376)),d_data(1:439),128);
  if a_control(4)==1
    d_var1(439+(1:376))=4*6/16*abs(d_var1(439+376+(1:376))+j*d_var2(439+376+(1:376)));
    d_var2(439+(1:376))=d_var1(439+(1:376));
  end
else
  d_data(94+(1:31))=remove_stripe(d_data(94+(1:31)),d_data(1:94),128);
  if a_control(4)==1
    d_var1(94+(1:31))=4*6/16*abs(d_var1(94+31+(1:31))+j*d_var2(94+31+(1:31)));
    d_var2(94+(1:31))=d_var1(94+(1:31));
  end
  ch_height=292.9;
end
