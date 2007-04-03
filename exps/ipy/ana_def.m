% Analysis defaults
a_satch.clutter=[30];
a_satch.sigma=4;
%a_satch.plot=8;
a_satch.repair=[Inf];
a_satch.cut=1;
d=find(analysis_altit>455); analysis_altit(d(1))=600;
Magic_const=1.87;
if strfind(data_path,'32p')
 analysis_do=0;
 analysis_txpower=8;
 d_saveint.dir=result_path;
 d_saveint.var=0;
 d=strfind(d_saveint.dir,'AUTO');
 analysis_intfixforce(4)=400;
 analysis_intfix(5:6)=47:48;
 d_saveint.range=20+col(ones(3*50,1)*(0:1)*19898+(1:3*50)'*ones(1,2)+22*50+21*768);
 if length(d)==1
  d_saveint.dir=[d_saveint.dir(1:d-1) sprintf('%s_%s%d_%d@32p',datestr(analysis_start,29),name_expr,expver,analysis_integr(1))];
 end
end
