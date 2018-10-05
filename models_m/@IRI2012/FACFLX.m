function [ FFAC ] = FACFLX( EE,UVFAC )
%FACFLX solar UVFAC factors
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE FACFLX(EE,UVFAC,FFAC)
%....... solar UVFAC factors. Correspond to the first 9 wavelengths
%....... TORR et al.[1979] GRL page 771 table 3. UVFAC(9) is for 304A

%      REAL UVFAC(59)
  FFAC=(7*UVFAC(9)+UVFAC(8)+0.2*UVFAC(6))/8.2;
  if(EE > 30 && EE <= 38)
    FFAC=(2*UVFAC(7)+.5*UVFAC(5))/2.5;
  end
  if(EE > 38 && EE <= 45)
    FFAC=UVFAC(4);
  end
  if(EE > 45 && EE <= 66)
    FFAC=UVFAC(3);
  end
  if(EE > 66 && EE <= 108)
    FFAC=UVFAC(2);
  end
  if(EE > 108)
    FFAC=UVFAC(1);
  end
end

