function testKnownValues(testCase)
  inputs ={' 40.,-102.,  -1.,2012,123,12.4, 0.2,104.,105.,4.9,5.0,4.4,4.1,4.0,4.2,4.3', ...
           '-20., 102.,  10.,2012,350,12.4,22.3,104.,105.,4.9,5.0,4.4,4.1,4.0,4.2,4.3', ...
           '  0., 280.,  60.,2012, 10, 0.1, 8.0,104.,105.,4.9,5.0,4.4,4.1,4.0,4.2,4.3', ...
           ' 90.,   0.,1001.,2012,200,13.3, 0.0,104.,105.,4.9,5.0,4.4,4.1,4.0,4.2,4.3'};
  tinf = [1027.31848145,1027.31848145,1027.31848145,994.59594727];
  ta = [291.46743774,237.04345703,250.89538574,994.59594727];
  da = { ...
    [0.14969919E+15,0.00000000E+00,0.22307470E+20,0.59844246E+19,0.26683080E+18,0.13724625E-02,0.00000000E+00,0.00000000E+00,0.00000000E+00], ...
    [0.45311683E+14,0.00000000E+00,0.67521344E+19,0.18113950E+19,0.80765657E+17,0.41542362E-03,0.00000000E+00,0.00000000E+00,0.00000000E+00], ...
    [0.34317605E+11,0.00000000E+00,0.51138484E+16,0.13718920E+16,0.61169298E+14,0.31462844E-06,0.00000000E+00,0.00000000E+00,0.00000000E+00], ...
    [0.35936637E+05,0.40497920E+04,0.55965072E+00,0.13474727E-02,0.55367114E-07,0.41680530E-18,0.37311246E+05,0.37266797E+03,0.19425848E+05], ...
    };
  tz = [211.96731567,203.81536865,218.72143555,994.59594727];
  dz = [0.90776711E+15,0.86949043E+15,0.89940917E+15,0.50892210E+00];
  g28 = [0.15896544,0.26260477,0.00923668,-0.04440383];
  dlbs = [0.38249103E+12,0.42425919E+12,0.32930241E+12,0.31210383E+12];
  ss = [0.23037642E-01,0.22690190E-01,0.23955621E-01,0.25115101E-01];
  tlbs = [0.33484372E+03,0.32423999E+03,0.36137943E+03,0.38458279E+03];
  tt = {[-0.37422150E-01  0.00000000E+00 -0.45779910E-01  0.98283336E-01  0.70685908E-01 -0.00000000E+00  0.25311530E-01  0.46409264E-01  0.00000000E+00  0.00000000E+00 -0.00000000E+00  0.00000000E+00 -0.00000000E+00  0.14774681E-02  0.00000000E+00], ...
        [-0.37422150E-01  0.00000000E+00  0.54383699E-01 -0.76144241E-01  0.59309378E-01 -0.00000000E+00  0.30507322E-01  0.20005803E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.31912751E-01  0.00000000E+00], ...
        [-0.37422150E-01  0.00000000E+00  0.45460757E-01 -0.12646082E+00 -0.00000000E+00  0.00000000E+00 -0.21400742E-01  0.13981938E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.92402548E-02  0.00000000E+00], ...
        [-0.37422150E-01  0.00000000E+00 -0.41049559E-01 -0.12438824E+00  0.15845613E+00  0.00000000E+00 -0.21350312E-07  0.32130840E-13  0.00000000E+00  0.00000000E+00 -0.00000000E+00 -0.00000000E+00 -0.00000000E+00 -0.24196683E-20  0.00000000E+00]};
  mass = 48;
  N = length(inputs);
  ctx = CIRA();
  apx = zeros(1,7);
  for i=1:N
    sc = textscan(inputs{i},'%f,');
    ydx = sc{1}(4)*1000. + sc{1}(5);
    secx = sc{1}(6)*3600.;
    stlx = sc{1}(7);
    latx = sc{1}(1);
    lonx = sc{1}(2);
    hx = sc{1}(3);
    f107ax = sc{1}(8);
    f107x = sc{1}(9);
    for j=1:7
      apx(j) = sc{1}(9+j);
    end
    [D,T,ctx] = ctx.GTD7(ydx,secx,hx,latx,lonx,stlx,f107ax,f107x,apx,mass);
    [G28,ctx,TL]=ctx.GLOBE7(ydx,secx,latx,lonx,stlx,f107ax,f107x, ...
     apx,ctx.PDA,CIRA.N2_MASS);
    dlb = ctx.PDM(1,CIRA.N2_MASS)*exp(G28(1,CIRA.Der0))*ctx.PDA(1,CIRA.N2_MASS);
    [DZ,TZ,ctx.TN1,ctx.TGN1] = ctx.DENSU(hx,dlb,T(CIRA.EXOSPHERIC_TEMP),ctx.GTS7_TLB,ctx.MT(CIRA.N2_MASS),0.1, ...
      ctx.PTM(6),ctx.S,ctx.MN1,ctx.ZN1,ctx.TN1,ctx.TGN1);
    str = sprintf('TINF(%d) %f ~= %f (%e)',i,T(1),tinf(i),(T(1)-tinf(i))/tinf(i));
    testCase.verifyEqual(T(1),tinf(i),'RelTol',6.0e-4,str);
    str = sprintf('TEMP(%d) %f ~= %f (%e)',i,T(2),ta(i),(T(2)-ta(i))/ta(i));
    testCase.verifyEqual(T(2),ta(i),'RelTol',6.0e-4,str);
    str = sprintf('TLB(%d) %f ~= %f (%e)',i,ctx.GTS7_TLB(1,1),tlbs(i),(ctx.GTS7_TLB(1,1)-tlbs(i))/tlbs(i));
    testCase.verifyEqual(ctx.GTS7_TLB(1,1),tlbs(i),'RelTol',6.0e-4,str);
    str = sprintf('S(%d) %f ~= %f (%e)',i,ctx.S(1,1),ss(i),(ctx.S(1,1)-ss(i))/ss(i));
    testCase.verifyEqual(ctx.S(1,1),ss(i),'RelTol',6.0e-4,str);
    str = sprintf('DLB(%d) %e ~= %e (%e)',i,dlb,dlbs(i),(dlb-dlbs(i))/dlbs(i));
    testCase.verifyEqual(dlb,dlbs(i),'RelTol',2.0e-3,str);
    for j=1:9
      if da{i}(j) == 0
        str = sprintf('DENS(%d,%d) %e ~= %e (%e)',i,j,D(j),da{i}(j),(D(j)-da{i}(j)));
        testCase.verifyEqual(D(j),da{i}(j),'AbsTol',5.0e-13,str);
      else
        str = sprintf('DENS(%d,%d) %e ~= %e (%e)',i,j,D(j),da{i}(j),(D(j)-da{i}(j))/da{i}(j));
        testCase.verifyEqual(D(j),da{i}(j),'RelTol',8.0e-3,str);
      end
    end
    str = sprintf('TZ(%d) %f ~= %f (%e)',i,TZ(1,CIRA.Der0),tz(i),(TZ(1,CIRA.Der0)-tz(i))/tz(i));
    testCase.verifyEqual(TZ(1,CIRA.Der0),tz(i),'RelTol',6.0e-4,str);
    str = sprintf('DZ(%d) %e ~= %e (%e)',i,DZ(1,CIRA.Der0),dz(i),(DZ(1,CIRA.Der0)-dz(i))/dz(i));
    testCase.verifyEqual(DZ(1,CIRA.Der0),dz(i),'RelTol',7.0e-2,str);
    str = sprintf('G28(%d) %f ~= %f (%e)',i,G28(1,CIRA.Der0),g28(i),(G28(1,CIRA.Der0)-g28(i))/g28(i));
    testCase.verifyEqual(G28(1,CIRA.Der0),g28(i),'RelTol',7.0e-3,str);
    for j=1:15
      if tt{i}(j) == 0
        str = sprintf('TL(%d,%d) %e ~= %e (%e)',i,j,TL(j),tt{i}(j),(TL(j)-tt{i}(j)));
        testCase.verifyEqual(TL(j),tt{i}(j),'AbsTol',5.0e-13,str);
      else
        str = sprintf('TL(%d,%d) %e ~= %e (%e)',i,j,TL(j),tt{i}(j),(TL(j)-tt{i}(j))/tt{i}(j));
        if abs(tt{i}(j)) < 1.0e-7
          testCase.verifyEqual(TL(j),tt{i}(j),'RelTol',7.0e-1,str);
        else
          testCase.verifyEqual(TL(j),tt{i}(j),'RelTol',3.0e-2,str);
        end
      end
    end
  end
end
