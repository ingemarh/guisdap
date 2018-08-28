function testPolynomials(testCase)
% test the spline functions and their derivatives
  N = 6;
  M = N;
  LAT = 1.1;
  W = 1.5;
  [ PLG, PLGdLat ] = CIRA.LegendrePolynomials( LAT, W, N, M );
  del = 0.0001;
  [ PLG1 ] = CIRA.LegendrePolynomials( LAT+del, W, N, M );
  dPLG = (PLG1-PLG)/del;
  for i=1:N
    for j=1:i
      testCase.verifyEqual(dPLG(i,j),PLGdLat(i,j),'AbsTol',5.0e-2);
    end
  end
  testCase.verifyEqual(PLG(2,1),sin(LAT*W),'AbsTol',1.0e-11);
  [ PCH, PCHdLat ] = CIRA.ChebyshevPolynomials( LAT, W, N );
  [ PCH1 ] = CIRA.ChebyshevPolynomials( LAT + del, W, N );
  dPCH = (PCH1-PCH)/del;
  for i=1:N
    for j=1:2
      testCase.verifyEqual(dPCH(i,j),PCHdLat(i,j),'AbsTol',5.0e-3);
    end
  end
  testCase.verifyEqual(PCH(2,1),cos(LAT*W),'AbsTol',1.0e-11);
end
