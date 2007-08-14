% Analysis defaults
a_satch.sigma=4;
a_satch.clutter=[32 32 16];
a_satch.cut=1;
%a_satch.plot=8;
if expver>1
  a_satch.clutter=[33 34 19];
  a_satch.repair=[104 29 0];
end
if strfind(data_path,'32p')
 analysis_do=0;
 analysis_txpower=8;
 d_saveint.dir=result_path;
 d_saveint.var=0;
 d=strfind(d_saveint.dir,'AUTO');
 if expver==2
  analysis_intfixforce=[NaN NaN NaN 160];
  d_saveint.range=col(ones(3*175,1)*(0:3)*19619+(1:3*175)'*ones(1,4)+10*175+9*1536);
 end
 if length(d)==1
  d_saveint.dir=[d_saveint.dir(1:d-1) sprintf('%s_%s%d_%d@32p',datestr(analysis_start,29),name_expr,expver,analysis_integr(1))];
 end
end
