function [ UVFAC ] = FACSR( UVFAC,F107,F107A )
%FACSR The Schumann-Runge factors
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE FACSR(UVFAC,F107,F107A)
%........ The Schumann-Runge factors are scaled according to F10.7
%........ from Torr et al. GRL 1980 p6063

%      IMPLICIT NONE
%      INTEGER I,LSR
%      REAL UVFAC(59),SRFLUX(8),SRA(8),SRB(8),F107,F107A
      %............. Schumann-Runge scaling
  persistent SRFLUX SRA SRB;
  if isempty(SRFLUX)
    SRFLUX = [2.4,1.4,.63,.44,.33,.17,.12,.053];
    %...... first two SRA and SRB values out of order in Marsha's paper
    SRA = [25.5,20.7,13.2,11.6,11.3,7.86,7.68,4.56];
    SRB = [222.,129.,53.4,36.0,25.0,11.3,6.35,2.05];
  end
  %
  %----  Test to see if need to scale - see DATRD2 subroutine      
      %if(floor(UVFAC(58)) == -1 || floor(UVFAC(58)) == -3)
  %
  for I=38:50
    LSR=I-37;
    UVFAC(I)=1.0;
    if LSR <= 8
      UVFAC(I)=(SRA(LSR)*1.0E7*F107+SRB(LSR)*1.0E9)/SRFLUX(LSR) ...
         /1.0E11;
    end
  end

end

