% integr_NW: Interface to Nigel Wade's integration package (mex call)
% 
% Interface to Nigel Wade's integration package version 2.
% Parameters:
% OK  : if true, the integration was succesful
% EOF : if true, the end of file was found during integration
% global parameters:
% d_parbl : the parameter block returned by the integration program
% d_data : the integrated data vector
% d_var1 d_var2: data variances
%
% iHandle: opaque data structure set when the integration is initialised
%          This must *not* be set before calling an_start.
% source: Either the name of file containing the source definition or
%         the actual source definition.
%         This must be set *after* calling start_GUP and before an_start
% strategy: Either the name of a file containing the integration strategy definition
%           or the actual strategy definition.
%         This must be set *after* calling start_GUP and before an_start
% trace_level: an integer which controls the level of trace messages output,
%              the higher the number the more verbose the messages.
%         This must be set *after* calling start_GUP and before an_start
%         If it is not defined then no trace messages are output.
%
% See also: an_start integr_data integrate
%
% function [OK,EOF]=integr_NW
%

function [OK,EOF]=integr_NW

global d_parbl d_data d_var1 d_var2 a_control 
global iHandle source strategy

[status,dHandle,modes] = PI_next(iHandle,1);

EOF=0;
if status ~= 0
  es=error_string(status);
  if ~strcmp( es, 'no more data in source') & ~strcmp( es, 'integration complete' )
    fprintf(' Error in integration - %s \n',error_string(status)); 
    OK=0; return
  end
  OK=0;
  EOF=1;
  return
end

if modes < 1
   OK=0; EOF=0; return
end


[status1,data]=PI_mode_data(dHandle,0);
[status2,var1,var2]=PI_mode_var(dHandle,0);
[status3,n_records,count]=PI_mode_count(dHandle,0);
[status4,d_parbl] = PI_parblk(dHandle);

[status]=PI_free(dHandle);

if status1 ~= 0 | status2 ~= 0 | status3 ~= 0 | status4 ~= 0
  fprintf(' error extracting data from data handle\n')
  OK=0; EOF=0; return
end

if isempty(var1) | isempty(var2)
  fprintf(' no variances in the data\n')
  OK=0; EOF=0; return
end

if isempty(count)
  total = n_records;
else
  total = count;
end

if a_control(4)==1 & n_records<5
  if n_records<2
    fprintf(' One file is not enough for variance determination\n')
    fprintf(' Skipping this integration period\n')
    fprintf(' command ''analysis_control(4)=2'' in the startup file will enable the analysis\n')
    OK=0; return
  end
  fprintf(' *******************    WARNING    **********************************\n')
  fprintf(' %.0f files may not be enough for reliable variance determination\n',n_records)
  fprintf(' *******************    WARNING    **********************************\n')
end


d_data = n_records * data(:) ./ total;
d_var1 = var1(:) - d_data .* d_data ./ total;
d_var2 = var2(:) - d_data .* conj(d_data) ./ total;

OK=1;
