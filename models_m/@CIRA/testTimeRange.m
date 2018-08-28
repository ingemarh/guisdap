function testTimeRange(testCase)
  numPoints = 100;
  startYear = 1900;
  endYear = 2015;
  xlat = 40;
  xlon = -104;
  hour = 0.0;
  DAYNR = 4.0;
  stl = 0.0;
  sec=hour*3600.;
  f107a = 152.0;
  f107 = 153.0;
  ap = zeros(CIRA.maxAP,1);
  ap(CIRA.DAILY_AP) = 4.1;
  ap(CIRA.CURRENT_AP) = 4.2;
  ap(CIRA.CURRENT_M_3_AP) = 4.3;
  ap(CIRA.CURRENT_M_6_AP) = 4.4;
  ap(CIRA.CURRENT_M_9_AP) = 4.5;
  ap(CIRA.CURRENT_M_12_33_AP) = 4.6;
  ap(CIRA.CURRENT_M_36_57_AP) = 4.7;
  hxx = 0.0;
  inc = (endYear-startYear)/(numPoints-1);
  press = zeros(numPoints,1);
  years = zeros(numPoints,1);
  ciractx = CIRA();
  ciractx = ciractx.TSELEC(CIRA.allSwitchesOn);
  mass = ciractx.MT(CIRA.ALL_MASS);
  i = 1;
  for YEAR = startYear:inc:endYear
    years(i) = YEAR;
    iyd = YEAR *CIRA.YRDSHIFT + DAYNR;
    [ D,T,ciractx ] = ciractx.GTD7(iyd,sec,hxx,xlat,xlon,stl,f107a,f107,ap,mass,0.0);
    press(i) = ciractx.totalPressure(D,T);
    i = i + 1;
  end
  figure;
  plot(years,press);
  title('History of Pressure');
  xlabel('Year');
  ylabel('Pressure (mb)');
  testCase.verifyEqual(YEAR,endYear);
end
