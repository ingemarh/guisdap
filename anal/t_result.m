% t_result: prints timing information
% GUISDAP v.1.80 96-05-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% See also: t_init
% function t_result
  function t_result
  
global ti_start ti_stop fl_start fl_stop ti_calls

ind=find(ti_start>0);
ajat=ti_stop(ind)-ti_start(ind);
cumcalls=fliplr(cumsum(fliplr(ti_calls(ind))));

%ajat=ajat-0.0186*cumcalls+0.0091*ti_calls(ind)  
%ajat=ajat-0.0244*cumcalls+0.012*ti_calls(ind)  MacIIci

len=length(ti_stop);
NN=50;t=cputime;for i=1:NN;t_start(len);t_stop(len);end  %AH 94-6-14 
time=(cputime-t)/NN;  %AH 94-6-14 
ajat=ajat-time*cumcalls+(time/2)*ti_calls(ind);

flo=(fl_stop(ind)-fl_start(ind))/1000;
for i=1:length(ind),
  fprintf('%1.0f ',i);
  fprintf('%9.3fs ',ajat(i));
  fprintf('%10.3f kflops ',flo(i));
  fprintf('%5.0f calls ',ti_calls(ind(i)));
  fprintf('%7.3f kflops/s\n',flo(i)/ajat(i));
end;
