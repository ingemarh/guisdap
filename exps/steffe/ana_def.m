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
 analysis_txpower=8;
 analysis_plasmaline=1;
 if expver==2
  analysis_intfixforce=[NaN NaN NaN 160];
  plasma_range=col(ones(3*175,1)*(0:3)*19619+(1:3*175)'*ones(1,4)+10*175+9*1536);
 end
end
