% Analysis defaults
%a_satch.sigma=4;
%a_satch.plot=8;
if name_site=='L'
  a_satch.cut=1;
  if strfind(data_path,'@32p')
    analysis_txpower=8;
    analysis_intfixforce=[NaN NaN NaN 960];
    analysis_intfix(5:6)=47:48;
    analysis_plasmaline=1;
    plasma_range=col(ones(2*125,1)*(0:1)*43509+(1:2*125)'*ones(1,2)+39259);
  else
    a_satch.clutter=[19];
    analysis_maxwidth=1.5*analysis_maxwidth;
    analysis_ppshortlags=1;
  end
elseif name_site=='T' | name_site=='V'
  a_satch.sigma=3;
  a_satch.cut=1;
  analysis_ppshortlags=1;
  if name_site=='T'
    if expver==1
      plasma_range=83111+(1:3*50);
    elseif expver==2
      plasma_range=col(ones(4*50,1)*(0:2)*82232+(1:4*50)'*ones(1,3)+108611);
    end
  else
    if expver==2
      plasma_range=col(ones(4*50,1)*(0:1)*82232+(1:4*50)'*ones(1,2)+83841);
    end
  end
  if local.site=='T', d_saveint.dir='/analysis/integrated/AUTO'; end
elseif name_site=='P'
 display_spectra=1;
 a_satch.do=0;
 analysis_range=[100:3:400];
 analysis_overlap=1;
 analysis_maxwidth=10;
 % a_freqband=0 for lowest (UHF) freq (-3.6MHz)pl-line band, 1 for 2nd (-6.0MHz), 2 for  3rd (-8.4MHz)
 a_freqband=1;   %set 20130228  MTR  
end
