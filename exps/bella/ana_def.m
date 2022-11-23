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
  a_satch.clutter=[2];
  a_satch.sigma=3;
  a_satch.cut=1;
  analysis_ppshortlags=1;
  if name_site=='T'
    analysis_maxwidth=1.5*analysis_maxwidth;
    if expver==1
      plasma_range=col(ones(5*75,1)*(0:3)*8087+(1:5*75)'*ones(1,4)+40114);
      if datenum(analysis_start)>datenum(2022,4,20)
        plasma_range=col(ones(5*75,1)*(0:1)*8087+(1:5*75)'*ones(1,2)+40114);
      end
    end
  elseif name_site=='V'
    plasma_range=col(ones(5*75,1)*(0:1)*14473+(1:5*75)'*ones(1,2)+14098);
  end
  if local.site=='T', d_saveint.dir='/analysis/integrated/AUTO'; end
elseif (name_site=='K' | name_site=='S') & analysis_start(1)>2010
  Magic_const=2; % single polarisation
  analysis_code=1;
end
