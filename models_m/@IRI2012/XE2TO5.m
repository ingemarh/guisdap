function [ XE2TO5,dXE2TO5 ] = XE2TO5( H,HMF2,NL,HX,SC,AMP )
%XE2TO5 NORMALIZED ELECTRON DENSITY (N/NMF2) FOR THE MIDDLE IONOSPHERE
%
%        FUNCTION XE2TO5(H,HMF2,NL,HX,SC,AMP)
%----------------------------------------------------------------------
% NORMALIZED ELECTRON DENSITY (N/NMF2) FOR THE MIDDLE IONOSPHERE FROM 
% HME TO HMF2 USING LAY-FUNCTIONS.
%----------------------------------------------------------------------

%        DIMENSION       HX(NL),SC(NL),AMP(NL)
  SUM = 1.0;
  dSUM = 0.0;
  for I=1:NL
    [YLAY,dYLAY] = IRI2012.RLAY( H, HMF2, SC(I), HX(I) );
    YLAY = AMP(I) * YLAY;
    dYLAY = AMP(I) * dYLAY;
    dSUM = dSUM + dYLAY;
    zlay=10.^YLAY;
    SUM=SUM*zlay;
  end
  XE2TO5 = SUM;
  dXE2TO5 = SUM*log(10.0)*dSUM;

end

