% Analysis defaults
a_satch.clutter=[30];
a_satch.sigma=4;
%a_satch.plot=8;
a_satch.repair=[Inf];
a_satch.cut=1;
analysis_ppshortlags=1;
d=find(analysis_altit>455); analysis_altit(d(1))=600;
global spearcount, spearcount='/analysis/results/spearcount';
if strfind(data_path,'@32p')
 analysis_txpower=8;
 analysis_intfixforce=[NaN NaN NaN 400];
 analysis_intfix(5:6)=47:48;
 if name_site=='P'
  display_spectra=1;
  a_satch.do=0;                
  analysis_range=[100:4.5:300];
  if expver==2
   analysis_range=[130.8-2.25:4.5:274.8+2.25];
  end
  analysis_overlap=1;
  analysis_maxwidth=10;
 else
  analysis_plasmaline=1;
  if expver==1
   plasma_range=20+col(ones(3*50,1)*(0:1)*19898+(1:3*50)'*ones(1,2)+22*50+21*768);
  elseif expver==2
   plasma_range=20+col(ones(3*50,1)*(0:1)*29814+(1:3*50)'*ones(1,2)+34*50+33*768);
  elseif expver==2
   plasma_range=20+col(ones(4*75,1)*(0:1)*66545+(1:4*75)'*ones(1,2)+51*75+50*1152);
  else
   plasma_range=20+col(ones(4*75,1)*(0:1)*87377+(1:4*75)'*ones(1,2)+67*75+66*1152);
  end
 end
elseif analysis_end(1)<2009
 analysis_intfix(5)=67;
 analysis_intfixforce=[NaN NaN NaN NaN 0];
 analysis_intallow=[.11 .11 1000 0 1];
end
