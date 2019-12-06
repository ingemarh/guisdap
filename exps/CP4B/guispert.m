if strcmp(name_expr,'CP4B') & ch_el(1)==0,
  fprintf(' Antenna elevation is zero...  Changing to the correct value\n')
  ch_el = [30.0*ones(size(ch_el))];
end
if exist('a_code')
  phased=(a_code(1)-1)/2;
  [ch_el ch_az ch_gain]=vhf_elaz(ch_el,phased*12,10^4.31/2);
  global vhf_tx
  ch_Pt=vhf_tx(phased+1);
end
