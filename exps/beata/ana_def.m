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
  plasma_range=83111+(1:3*25);
end
