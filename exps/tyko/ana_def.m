% Analysis defaults
a_satch.clutter=21;
a_satch.sigma=4;
%a_satch.plot=8;
a_satch.repair=[Inf];
a_satch.cut=1;
analysis_ppshortlags=1;
d=find(analysis_altit>555); analysis_altit(d(1))=700;
if strfind(data_path,'@32p')
 analysis_txpower=8;
 analysis_intfix(5:6)=47:48;
 analysis_plasmaline=1;
end
