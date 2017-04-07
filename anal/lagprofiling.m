function lagprofiling()
global d_data d_raw d_parbl a_lagprofiling
for lpf=a_lagprofiling'
 if strcmp(lpf.lib,'plwin')
  [dd_data,upar]=plwin(lpf.par,d_parbl,d_raw(lpf.raw));
 end 
 d_data(lpf.data+(1:length(dd_data)))=dd_data;
end
return
