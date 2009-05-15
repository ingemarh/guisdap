% guispert.m: special experiment specific hacks
% GUISDAP v8.5   06-05-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if name_site=='V'
 [ch_el ch_az ch_gain]=vhf_elaz(ch_el(1),0,10^4.31/2);
elseif name_site=='L'
 if length(d_data)>11370
  calTemp=[163*ones(1,17) 228*ones(1,17)];
  if all(ch_gain==ch_gain(1))
    if isempty(a_code)
      a_satch.clutter=repmat(a_satch.clutter,1,2);
      a_satch.repair=repmat(a_satch.repair,1,2);
    end
    ch_gain(3:4)=ch_gain(3:4)*0.8;
  end
 end
 %glp=1182;
 %grps=[1 1 lpg_h(1);2 1180 lpg_h(1)+lpg_w(1)/2
 %     1181 1182 lpg_h(1182)];
 %gaincorrect(glp,grps)
end
