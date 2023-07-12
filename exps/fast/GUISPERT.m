%if strcmp(name_expr,'fast') & ch_el(1)<75,
if strcmp(name_expr,'fast'),
  fprintf(' Changing elevations for every channel...\n')
  ch_el = [70 90 70 90 70 90 70 90];
end

