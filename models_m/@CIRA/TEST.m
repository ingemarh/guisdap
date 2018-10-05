function [ rtn ] = TEST( input, testCase )
%TEST This function tests the CIRA class for NaNs and numerical derivative comparisons

  rtn = 0;
  output = 1;
  sc = textscan(input,'%f');
  sc = sc{1};

  xlat = sc(1);
  xlon = sc(2);
  iy = sc(3);
  DAYNR = sc(4);
  hour = sc(5);
  hxx = sc(6);

  sec=hour*3600.;
  iyd = iy *CIRA.YRDSHIFT + DAYNR;
  stl = sc(7);
  f107a = sc(8);
  f107 = sc(9);
  ap = zeros(CIRA.maxAP,1);
  ap(CIRA.DAILY_AP) = sc(10);
  ap(CIRA.CURRENT_AP) = sc(11);
  ap(CIRA.CURRENT_M_3_AP) = sc(12);
  ap(CIRA.CURRENT_M_6_AP) = sc(13);
  ap(CIRA.CURRENT_M_9_AP) = sc(14);
  ap(CIRA.CURRENT_M_12_33_AP) = sc(15);
  ap(CIRA.CURRENT_M_36_57_AP) = sc(16);
  ciractx = CIRA();
  mass = ciractx.MT(CIRA.ALL_MASS);
  % try all switches
  SWMI = zeros(CIRA.maxSW,1);
  for i=1:CIRA.maxSW
    for j=-1:1
      SWMI(i)=j;
      ciractx = ciractx.TSELEC(SWMI);

      [D,T,ciractx] = ciractx.GTD7(iyd,sec,hxx,xlat,xlon,stl,f107a,f107,ap,mass,0.0);
      for k=1:CIRA.numTemperatures
        if isnan(T(k))
          if nargin > 1
            testCase.verifyFalse(isnan(T(k)));
          end
          rtn = rtn + 1;
          fprintf(output,'GTD7 T: %15.4e %15.4e %15.4e\n', ...
             T(CIRA.EXOSPHERIC_TEMP),T(CIRA.TemperatureIndex(CIRA.Der0)), ...
             T(CIRA.TemperatureIndex(CIRA.DerAlt)));
        end
      end
      for k=1:CIRA.numDensities
        if isnan(D(k))
          if nargin > 1
            testCase.verifyFalse(isnan(D(k)));
          end
          rtn = rtn + 1;
          fprintf(output,'GTD7 D: %15.4e %15.4e %15.4e %15.4e %15.4e %15.4e %15.4e %15.4e\n', ...
             D(CIRA.HE_DENS),D(CIRA.O_DENS),D(CIRA.N2_DENS),D(CIRA.O2_DENS), ...
             D(CIRA.AR_DENS),D(CIRA.TOTAL_DENS),D(CIRA.H_DENS),D(CIRA.N_DENS));
          fprintf(output,'GTD7 GD: %15.4e %15.4e %15.4e %15.4e %15.4e %15.4e %15.4e %15.4e\n', ...
             D(CIRA.HE_GDENS),D(CIRA.O_GDENS),D(CIRA.N2_GDENS),D(CIRA.O2_GDENS), ...
             D(CIRA.AR_GDENS),D(CIRA.TOTAL_GDENS),D(CIRA.H_GDENS),D(CIRA.N_GDENS));
        end
      end
    end
  end
  SWMI(5)=0;
  ciractx = ciractx.TSELEC(SWMI);
  [D,T,ciractx] = ciractx.GTD7(iyd,sec,hxx,xlat,xlon,stl,f107a,f107,ap,mass,0.0);
  del = 0.0001;
  [D2,T2,ciractx] = ciractx.GTD7(iyd,sec,hxx+del,xlat,xlon,stl,f107a,f107,ap,mass,0.0);
  dT2 = (T2-T)/del;
  dD2 = (D2-D)/del;
  del = 0.0001;
  [D3,T3,ciractx] = ciractx.GTD7(iyd,sec,hxx,xlat,xlon+del,stl+del/15,f107a,f107,ap,mass,0.0);
  dT3 = (T3-T)/del;
  dD3 = (D3-D)/del;
  del = 0.0001;
  [D4,T4,ciractx] = ciractx.GTD7(iyd,sec,hxx,xlat+del,xlon,stl,f107a,f107,ap,mass,0.0);
  dT4 = (T4-T)/del;
  dD4 = (D4-D)/del;
  del = 0.0001;
  [D5,T5,ciractx] = ciractx.GTD7(iyd,sec+del,hxx,xlat,xlon,stl+del/3600,f107a,f107,ap,mass,0.0);
  dT5 = (T5-T)/del;
  dD5 = (D5-D)/del;
  rtn = rtn + testTemp(dT2,T,CIRA.DerAlt,'ALT',testCase);
  rtn = rtn + testTemp(dT3,T,CIRA.DerLon,'LON',testCase);
  rtn = rtn + testTemp(dT4,T,CIRA.DerLat,'LAT',testCase);
  rtn = rtn + testTemp(dT5,T,CIRA.DerSec,'TIME',testCase);
  rtn = rtn + testDensity(dD2,D,CIRA.DerAlt,'ALT',testCase);
  rtn = rtn + testDensity(dD3,D,CIRA.DerLon,'LON',testCase);
  rtn = rtn + testDensity(dD4,D,CIRA.DerLat,'LAT',testCase);
  rtn = rtn + testDensity(dD5,D,CIRA.DerSec,'TIME',testCase);
  del = 0.001;
  DB28 = zeros(1,CIRA.DerLast);
  TINF = zeros(1,CIRA.DerLast);
  TLB = zeros(1,CIRA.DerLast);
  ALT = zeros(1,CIRA.DerLast);
  ALT(1,CIRA.Der0) = hxx;
  ALT(1,CIRA.DerAlt) = 1.0;
  ALT1 = ALT;
  ALT1(1,CIRA.Der0) = hxx+del;
  ALT1(1,CIRA.DerAlt) = 1.0;
  DB28(1,CIRA.Der0) = 1.0e10;
  TINF(1,CIRA.Der0) = 1000.0;
  TLB(1,CIRA.Der0) = 950.0;
  MT = 28.1;
  ALPHA = 0.5;
  ZN1 = [40, 60, 70, 100, 200];
  MN1 = length(ZN1);
  [D1,T1,ciractx.TN1,ciractx.TGN1]=ciractx.DENSU(ALT,DB28,TINF,TLB,MT,ALPHA, ...
    ciractx.PTM(6),ciractx.S,MN1,ZN1,ciractx.TN1,ciractx.TGN1);
  [D2,T2,ciractx.TN1,ciractx.TGN1]=ciractx.DENSU(ALT1,DB28,TINF,TLB,MT,ALPHA, ...
    ciractx.PTM(6),ciractx.S,MN1,ZN1,ciractx.TN1,ciractx.TGN1);
  dT = (T2-T1)/del;
  if T2(1,CIRA.Der0) == T1(1,CIRA.Der0)
    eps = 5.0e-11;
    diff = (dT(1,CIRA.Der0)-T1(1,CIRA.DerAlt));
  else
    eps = 4.0e-4;
    diff = (dT(1,CIRA.Der0)-T1(1,CIRA.DerAlt))/T1(1,CIRA.DerAlt);
  end
  if abs(diff) > eps
    str = sprintf('DENSU T ALT deriv: %e ==? %e (%e) %e\n', ...
      dT(1,CIRA.Der0),T1(1,CIRA.DerAlt),diff,T2(1,CIRA.DerAlt));
    if nargin > 1
      testCase.verifyLessThan(abs(diff),eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
  dD = (D2-D1)/del;
  eps = 5.0e-4;
  diff = (dD(1,CIRA.Der0)-D1(1,CIRA.DerAlt))/D1(1,CIRA.DerAlt);
  if abs(diff) > eps
    str = sprintf('DENSU D ALT deriv: %e ==? %e (%e) %e\n\n',dD(1,CIRA.Der0),D1(1,CIRA.DerAlt),diff,D2(1,CIRA.DerAlt));
    if nargin > 1
      testCase.verifyEqual(dD(1,CIRA.Der0),D1(1,CIRA.DerAlt),'RelTol',eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
  [ TINF,ciractx ] = ciractx.GLOBE7( iyd,sec,xlat,xlon,stl,f107a,f107,ap,ciractx.PDA,CIRA.N_MASS );
  [ TINF2,ciractx ] = ciractx.GLOBE7( iyd,sec,xlat+del,xlon,stl,f107a,f107,ap,ciractx.PDA,CIRA.N_MASS );
  [ TINF3,~ ] = ciractx.GLOBE7( iyd,sec,xlat,xlon+del,stl,f107a,f107,ap,ciractx.PDA,CIRA.N_MASS );
  dT2 = (TINF2-TINF)/del;
  dT3 = (TINF3-TINF)/del;
  if TINF2(1,CIRA.Der0) == TINF(1,CIRA.Der0)
    eps = 5.0e-11;
    diff = (dT2(1,CIRA.Der0)-TINF(1,CIRA.DerLat));
  else
    eps = 9.0e-3;
    diff = (dT2(1,CIRA.Der0)-TINF(1,CIRA.DerLat))/TINF(1,CIRA.DerLat);
  end
  if abs(diff) > eps
    str = sprintf('GLOBE7 T LAT deriv: %e ==? %e (%e) %e\n',dT2(1,CIRA.Der0),TINF(1,CIRA.DerLat),diff,TINF2(1,CIRA.DerLat));
    if nargin > 1
      testCase.verifyLessThan(abs(diff),eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
  eps = 7.0e-4;
  diff = (dT3(1,CIRA.Der0)-TINF(1,CIRA.DerLon))/TINF(1,CIRA.DerLon);
  if abs(diff) > eps
    str = sprintf('GLOBE7 T Lon deriv: %e ==? %e (%e) %e\n\n',dT3(1,CIRA.Der0),TINF(1,CIRA.DerLon),diff,TINF3(1,CIRA.DerLon));
    if nargin > 1
      testCase.verifyEqual(dT3(1,CIRA.Der0),TINF(1,CIRA.DerLon),'RelTol',eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
end

function rtn = testTemp(dT,T,d,nm,testCase)
  rtn = 0;
  indx0 = CIRA.TemperatureIndex(CIRA.Der0);
  indx = CIRA.TemperatureIndex(d);
  if dT(indx0) == 0.0
    eps = 7.0e-11;
    diff = (dT(indx0)-T(indx));
  else
    eps = 5.0e-3;
    diff = (dT(indx0)-T(indx))/T(indx);
  end
  if abs(diff) > eps
    str = sprintf('GTD7 T %s deriv: %e ==? %e (%e)\n',nm, ...
      dT(indx0),T(indx),diff);
    if nargin > 3
      testCase.verifyLessThan(abs(diff),eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
end
function rtn = testDensity(dD2,D,d,nm,testCase)
  rtn = 0;
  eps = 8.0e-4;
  indx0 = CIRA.DensityIndex(CIRA.Der0,CIRA.ALL_MASS);
  indx = CIRA.DensityIndex(d,CIRA.ALL_MASS);
  diff2 = (dD2(indx0)-D(indx))/D(indx);
  if abs(diff2) > eps
    str = sprintf('GTD7 D %s deriv: %e ==? %e (%e)\n',nm, ...
      dD2(indx0),D(indx),diff2);
    if nargin > 1
      testCase.verifyEqual(dD2(indx0),D(indx),'RelTol',eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
  indx0 = CIRA.DensityIndex(CIRA.Der0,CIRA.N2_MASS);
  indx = CIRA.DensityIndex(d,CIRA.N2_MASS);
  diff2 = (dD2(indx0)-D(indx))/D(indx);
  eps = 8.0e-4;
  if abs(diff2) > eps
    str = sprintf('GTD7 D N2 %s deriv: %e ==? %e (%e)\n',nm, ...
      dD2(indx0),D(indx),diff2);
    if nargin > 1
      testCase.verifyEqual(dD2(indx0),D(indx),'RelTol',eps,str);
    else
      fprintf(output,str);
    end
    rtn = rtn + 1;
  end
end
