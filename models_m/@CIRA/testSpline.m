function testSpline(testCase)
% test the spline functions and their derivatives
  N = 4;
  XS = zeros(N);
  XS(1) = 1;
  XS(2) = 2;
  XS(3) = 3;
  XS(4) = 4;
  YS = zeros(N);
  YS(1) = 1;
  YS(2) = -1;
  YS(3) = 5;
  YS(4) = 4;
  YD1 = 0.5;
  YD2 = -0.5;
  X = 1.5;
  [Y2,Y2dXS,Y2dYS,Y2dYD1,Y2dYD2] = CIRA.SPLINEM(XS,YS,1,N,1,YD1,YD2);
  [Y,YdX,YdXS,YdYS,YdYD1,YdYD2] = CIRA.SPLINTM(XS,YS,Y2,1,N,1,X,Y2dXS,Y2dYS,Y2dYD1,Y2dYD2);
  [YI,YIdX,YIdXS,YIdYS,YIdYD1,YIdYD2] = CIRA.SPLINI(XS,YS,Y2,1,N,1,X,Y2dXS,Y2dYS,Y2dYD1,Y2dYD2);
  % derivative of XS
  del = 0.0001;
  for L=1:N
    XS1 = XS;
    XS1(L) = XS(L) + del;
    [Y21,Y2dXS1,Y2dYS1,Y2dYD11,Y2dYD21] = CIRA.SPLINEM(XS1,YS,1,N,1,YD1,YD2);
    for M=1:N
      testCase.verifyEqual((Y21(M)-Y2(M))/del,Y2dXS(M,L),'AbsTol',5.0e-3);
    end
    [Y1] = CIRA.SPLINTM(XS1,YS,Y21,1,N,1,X,Y2dXS1,Y2dYS1,Y2dYD11,Y2dYD21);
    testCase.verifyEqual((Y1-Y)/del,YdXS(L),'AbsTol',5.0e-4);
    [YI1] = CIRA.SPLINI(XS1,YS,Y21,1,N,1,X,Y2dXS1,Y2dYS1,Y2dYD11,Y2dYD21);
    testCase.verifyEqual((YI1-YI)/del,YIdXS(L),'AbsTol',5.0e-3);
  end
  % derivative of YS
  for L=1:N
    YS2 = YS;
    YS2(L) = YS(L) + del;
    [Y22,Y2dXS2,Y2dYS2,Y2dYD12,Y2dYD22] = CIRA.SPLINEM(XS,YS2,1,N,1,YD1,YD2);
    for M=1:N
      testCase.verifyEqual((Y22(M)-Y2(M))/del,Y2dYS(M,L),'AbsTol',5.0e-3);
    end
    [Y_2] = CIRA.SPLINTM(XS,YS2,Y22,1,N,1,X,Y2dXS2,Y2dYS2,Y2dYD12,Y2dYD22);
    testCase.verifyEqual((Y_2-Y)/del,YdYS(L),'AbsTol',5.0e-4);
    [YI2] = CIRA.SPLINI(XS,YS2,Y22,1,N,1,X,Y2dXS2,Y2dYS2,Y2dYD12,Y2dYD22);
    testCase.verifyEqual((YI2-YI)/del,YIdYS(L),'AbsTol',5.0e-3);
  end
  % derivative of YD1
  YD13 = YD1 + del;
  [Y23,Y2dXS3,Y2dYS3,Y2dYD13,Y2dYD23] = CIRA.SPLINEM(XS,YS,1,N,1,YD13,YD2);
  for M=1:N
    testCase.verifyEqual((Y23(M)-Y2(M))/del,Y2dYD1(M),'AbsTol',5.0e-3);
  end
  [Y3] = CIRA.SPLINTM(XS,YS,Y23,1,N,1,X,Y2dXS3,Y2dYS3,Y2dYD13,Y2dYD23);
  testCase.verifyEqual((Y3-Y)/del,YdYD1,'AbsTol',5.0e-4);
  [YI3] = CIRA.SPLINI(XS,YS,Y23,1,N,1,X,Y2dXS3,Y2dYS3,Y2dYD13,Y2dYD23);
  testCase.verifyEqual((YI3-YI)/del,YIdYD1,'AbsTol',5.0e-3);
  % derivative of YD2
  YD23 = YD2 + del;
  [Y23,Y2dXS3,Y2dYS3,Y2dYD13,Y2dYD23] = CIRA.SPLINEM(XS,YS,1,N,1,YD1,YD23);
  for M=1:N
    testCase.verifyEqual((Y23(M)-Y2(M))/del,Y2dYD2(M),'AbsTol',5.0e-3);
  end
  [Y4] = CIRA.SPLINTM(XS,YS,Y23,1,N,1,X,Y2dXS3,Y2dYS3,Y2dYD13,Y2dYD23);
  testCase.verifyEqual((Y4-Y)/del,YdYD2,'AbsTol',5.0e-4);
  [YI4] = CIRA.SPLINI(XS,YS,Y23,1,N,1,X,Y2dXS3,Y2dYS3,Y2dYD13,Y2dYD23);
  testCase.verifyEqual((YI4-YI)/del,YIdYD2,'AbsTol',5.0e-3);
  % derivative of X
  X5 = X + del;
  [Y5] = CIRA.SPLINTM(XS,YS,Y2,1,N,1,X5,Y2dXS,Y2dYS,Y2dYD1,Y2dYD2);
  testCase.verifyEqual((Y5-Y)/del,YdX,'AbsTol',5.0e-4);
  [YI5] = CIRA.SPLINI(XS,YS,Y2,1,N,1,X5,Y2dXS,Y2dYS,Y2dYD1,Y2dYD2);
  testCase.verifyEqual((YI5-YI)/del,YIdX,'AbsTol',5.0e-3);
  
  testCase.verifyEqual(Y,-0.19166666666666667,'AbsTol',1.0e-11);
  testCase.verifyEqual(YI,0.28819444444444444,'AbsTol',1.0e-11);
  
  XS = zeros(N,CIRA.DerLast);
  XS(1,CIRA.Der0) = 1;
  XS(1,CIRA.DerAlt) = 0.1;
  XS(2,CIRA.Der0) = 2;
  XS(2,CIRA.DerAlt) = -0.1;
  XS(3,CIRA.Der0) = 3;
  XS(3,CIRA.DerAlt) = 0.2;
  XS(4,CIRA.Der0) = 4;
  XS(4,CIRA.DerAlt) = -0.2;
  YS = zeros(N,CIRA.DerLast);
  YS(1,CIRA.Der0) = 1;
  YS(1,CIRA.DerAlt) = 0.1;
  YS(2,CIRA.Der0) = -1;
  YS(2,CIRA.DerAlt) = -0.1;
  YS(3,CIRA.Der0) = 5;
  YS(3,CIRA.DerAlt) = 0.2;
  YS(4,CIRA.Der0) = 4;
  YS(4,CIRA.DerAlt) = -0.2;
  YD1 = zeros(1,CIRA.DerLast);
  YD1(1,CIRA.Der0) = 0.5;
  YD1(1,CIRA.DerAlt) = 0.1;
  YD2 = zeros(1,CIRA.DerLast);
  YD2(1,CIRA.Der0) = -0.5;
  YD2(1,CIRA.DerAlt) = -0.1;
  X = zeros(1,CIRA.DerLast);
  X(1,CIRA.Der0) = 1.5;
  X(1,CIRA.DerAlt) = 0.2;
  [Y,YI] = CIRA.SPLINE(XS,YS,1,N,1,YD1,YD2,X);
  testCase.verifyEqual(Y(1,CIRA.Der0),-0.19166666666666667,'AbsTol',1.0e-11);
  testCase.verifyEqual(Y(1,CIRA.DerAlt),-0.396916666667,'AbsTol',1.0e-11);
  testCase.verifyEqual(YI(1,CIRA.Der0),0.288194444444444,'AbsTol',1.0e-11);
  testCase.verifyEqual(YI(1,CIRA.DerAlt),0.003975694444444,'AbsTol',1.0e-11);
end
