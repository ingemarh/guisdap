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
 if expver<4
  analysis_intfixforce=[NaN NaN NaN 400];
 end
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
  elseif expver==3
   plasma_range=20+col(ones(4*75,1)*(0:1)*66545+(1:4*75)'*ones(1,2)+51*75+50*1152);
  elseif expver==4
   plasma_range=20+col(ones(4*75,1)*(0:1)*87377+(1:4*75)'*ones(1,2)+67*75+66*1152);
   if strfind(data_path,'4.1')
    plasma_range=20+col(ones(4*75,1)*(0:3)*86627+(1:4*75)'*ones(1,4)+67*75+66*1152);
   end
  end
 end
else
 if analysis_end(1)<2009
  analysis_intfix(5)=67;
  analysis_intfixforce=[NaN NaN NaN NaN 0];
  analysis_intallow=[.11 .11 1000 0 1];
 end
 if contains(data_path,'fixed42p')
  if expver==4
   analysis_lpf.par=load([path_expr 'ipy_lc.par4']);
   analysis_lpf.lib='clutter';
   analysis_lpf.raw=64*61;
   analysis_lpf.data=30;
   analysis_lpf.do=0;
   analysis_lpf(2).par=load([path_expr 'ipy_lac.par4']);
   analysis_lpf(2).lib='alt_decoder';
   analysis_lpf(2).raw=64*61;
   analysis_lpf(2).data=190;
   analysis_lpf(3)=analysis_lpf(1);
   analysis_lpf(3).raw=64*61*2+160*1600;
   analysis_lpf(3).data=30+10567;
   analysis_lpf(4)=analysis_lpf(2);
   analysis_lpf(4).raw=64*61*2+160*1600;
   analysis_lpf(4).data=190+10567;
  end
 end
end
