function [ XM3000HM, X ] = XM3000HM( XMAGBR,R,X,HMF2 )
%XM3000HM PROPAGATION FACTOR M3000 FOR THE MAGNETIC LATITUDE
%
%      REAL FUNCTION XM3000HM(XMAGBR,R,X,HMF2)         
%--------------------------------------------------------------
% CALCULATES THE PROPAGATION FACTOR M3000 FOR THE MAGNETIC LATITUDE
% XMAGBR/DEG. AND THE SMOOTHED ZUERICH SUNSPOT NUMBER R USING THE                        
% PEAK HEIGHT HMF2/KM AND THE RATIO X=FOF2/FOE. Reverse of HMF2ED.                      
% [REF. D.BILITZA ET AL., TELECOMM.J., 46, 549-553, 1979]                       
% D.BILITZA,1980. ----- no longer used    
%--------------------------------------------------------------
  F1=0.00232*R+0.222;
  F2=1.2-0.0116*exp(0.0239*R);
  F3=0.096*(R-25.0)/150.0;
  F4=1.0-R/150.0*exp(-XMAGBR*XMAGBR/1600.0);
  if X < 1.7
    X=1.7;
  end
  DELM=F1*F4/(X-F2)+F3;
  XM3000HM=1490.0/(HMF2+176.0)-DELM;

end

