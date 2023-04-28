analysis_adjust=[path_expr 'adjust_data' name_site];
if name_site=='T'
 analysis_maxwidth=3*analysis_maxwidth;
else
 analysis_adjust(end)='R';
end
