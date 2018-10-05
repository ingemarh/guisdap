function [ HMF2ED, X ] = HMF2ED( XMAGBR,R,X,XM3 )
%HMF2ED CALCULATES THE PEAK HEIGHT HMF2/KM
%
%      REAL FUNCTION HMF2ED(XMAGBR,R,X,XM3)         
%--------------------------------------------------------------
% CALCULATES THE PEAK HEIGHT HMF2/KM FOR THE MAGNETIC                           
% LATITUDE XMAGBR/DEGREE AND THE SMOOTHED ZUERICH SUNSPOT                         
% NUMBER R USING CCIR-M3000 XM3 AND THE RATIO X=FOF2/FOE.
% FOLLOWING CCIR RECOMMENDATION X IS LIMITED TO VALUE
% GREATER OR EQUAL TO 1.7 .                       
% [REF. D.BILITZA ET AL., TELECOMM.J., 46, 549-553, 1979]                       
% D.BILITZA,1980.     
%--------------------------------------------------------------
  F1=0.00232*R+0.222;
  F2=1.2-0.0116*exp(0.0239*R);
  F3=0.096*(R-25.0)/150.0;
  F4=1.0-R/150.0*exp(-XMAGBR*XMAGBR/1600.0);
  if X < 1.7
    X=1.7;
  end
  DELM=F1*F4/(X-F2)+F3;
  HMF2ED=1490.0/(XM3+DELM)-176.0;

end

