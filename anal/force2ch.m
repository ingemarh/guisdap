% force2ch.m: look for forced parameters
% GUISDAP v1.81   03-01-30 Copyright EISCAT
%
% See also: GUIZARD
forced=who('f_*');
for i=forced'
 forcepar=char(i);
 forcedpar=forcepar(3:end);
 if exist(forcedpar,'var')
  forcepare=eval(forcepar);
  if isstr(forcepare)
   try
    eval([forcedpar '=' forcepare ';'])
   catch
    eval([forcedpar '=' forcepar ';'])
   end
  else
   eval([forcedpar '=' forcepar ';'])
  end
 else
  disp(['Forced parameter ' forcedpar ' do not exists!'])
 end
end
