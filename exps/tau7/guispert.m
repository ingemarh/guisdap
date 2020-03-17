% guispert.m: special experiment specific hacks
% GUISDAP v8.2   03-01-30 Copyright EISCAT
%
% See also: GUISPERT GUIZARD
%
if name_site=='L'
 if length(d_data)>290942
  calTemp=[163 163 228 228];
  if all(ch_gain==ch_gain(1))
   if (isempty(a_code) | length(a_code)==2)
    a_satch.clutter=repmat(a_satch.clutter,1,2);
    a_satch.repair=repmat(a_satch.repair,1,2);
   end
   ch_gain(3:4)=ch_gain(3:4)*0.78; % for 4 march 2007
  end
 end
 ng=0; %length(ch_gain)/2;
 glp=5333;
 grps=[1 1 lpg_h(1);2 5331 lpg_h(1)+lpg_w(1)/2
       5333 5333 lpg_h(5333)];
 for i=1:ng
  gaincorrect(glp,grps)
  glp=glp+5333; grps(:,1:2)=grps(:,1:2)+5333;
 end
elseif name_site=='V'
 if expver==1
  ch_gain=ch_gain*sqrt(2); % Receiving on full face
 end
end
