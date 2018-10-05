function [ FOF1ED ] = FOF1ED( YLATI,R,CHI )
%FOF1ED CALCULATES THE F1 PEAK PLASMA FREQUENCY (FOF1/MHZ)
%
%      REAL FUNCTION FOF1ED(YLATI,R,CHI)
%--------------------------------------------------------------
% CALCULATES THE F1 PEAK PLASMA FREQUENCY (FOF1/MHZ)
% FOR   DIP-LATITUDE (YLATI/DEGREE)
%       SMOOTHED ZURICH SUNSPOT NUMBER (R)
%       SOLAR ZENITH ANGLE (CHI/DEGREE)
% REFERENCE: 
%       E.D.DUCHARME ET AL., RADIO SCIENCE 6, 369-378, 1971
%                                      AND 8, 837-839, 1973
%       HOWEVER WITH MAGNETIC DIP LATITUDE INSTEAD OF GEOMAGNETIC
%       DIPOLE LATITUDE, EYFRIG, 1979                    
%--------------------------------------------- D. BILITZA, 1988.   

%        COMMON/CONST/UMR
  FOF1ED=0.0;
  if CHI > 90.0
    return;
  end

  DLA =  YLATI;
  F0 = 4.35 + DLA * ( 0.0058 - 1.2E-4 * DLA );
  F100 = 5.348 + DLA * ( 0.011 - 2.3E-4 * DLA );
  FS = F0 + ( F100 - F0 ) * R / 100.0;
  XMUE = 0.093 + DLA * ( 0.0046 - 5.4E-5 * DLA ) + 3.0E-4 * R;
  FOF1 = FS * cos( CHI * IRI2012.UMR ) ^ XMUE;
  CHI0 = 49.84733 + 0.349504 * DLA;
  CHI100 = 38.96113 + 0.509932 * DLA;
  CHIM = ( CHI0 + ( CHI100 - CHI0 ) * R / 100. );
  if CHI > CHIM
    FOF1=-FOF1;
  end
  FOF1ED = FOF1;

end

