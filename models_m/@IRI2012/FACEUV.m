function [ UVFAC ] = FACEUV( UVFAC,F107,F107A )
%FACEUV This routine uses the EUV scaling
%:::::::::::::::::::::::::::::::: FACEUV :::::::::::::::::::::::
%      SUBROUTINE FACEUV(UVFAC,F107,F107A)
%----- This routine uses the EUV scaling from Richards et al.[1994]
%----- The EUVAC flux model is based on the F74113 solar reference
%----- spectrum and Hinteregger's scaling factors. This subroutine
%----- just provides the scaling factors as a function of the proxy
%----- (F107+F107A)/2

%      IMPLICIT NONE
%      INTEGER I
%      REAL UVFAC(59),HFG200(37),A,B,C,F107,F107A,F107AV
  persistent HFG200;
  if isempty(HFG200)
    HFG200 = [2.202,1.855,2.605,3.334,1.333,17.522,4.176,4.0 ...
        ,1.4,3.694,1.791,5.385,1.889,1.899,3.427,2.051,1.392,1.619 ...
        ,1.439,2.941,1.399,2.416,1.512,1.365,1.570,1.462,2.537,1.393 ...
        ,1.572,1.578,1.681,1.598,1.473,1.530,1.622,1.634,1.525];
  end

  %--  Test to see if need to scale - see DATRD2 subroutine      
  if floor(UVFAC(58)) == -1 || floor(UVFAC(58)) == -3
    %........... EUV scaling
    F107AV=(F107+F107A)*0.5;
    for I=1:length(HFG200)
      A=(HFG200(I)-1)/120.0;
      B=1-A*80.0;
      UVFAC(I)=A*F107AV+B;
      if UVFAC(I) < 0.8
        UVFAC(I)=0.8;
      end
    end
  end

end

