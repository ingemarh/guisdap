%if strcmp(name_expr,'fast') & ch_el(1)<75,
if strcmp(name_expr,'fastW'),
  fprintf(' Changing elevations for every channel...\n')
  ch_el = ones(1,8)*90;
end

