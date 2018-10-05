function [ SIGIT ] = TXSION( E )
%TXSION total ionization cross sections for O, O2, and N2
%::::::::::::::::::::::: TXSION ::::::::::::::::::::::::::::::::::
%..... total ionization cross sections for O, O2, and N2
%..... ionization cross sections keiffer and dunn ........
%..... The N2+ and O2+ cross sections were modified in April 99 to agree
%..... with the Schram et al. cross sections at high energies
%      SUBROUTINE TXSION(E,SIGIT)

%      DIMENSION SIGIT(3)
  SIGIT = zeros(3,1);
  %... SIGTMP is used for N2+ and O2+ at the high energies
  SIGTMP=1.0E-13*exp(-2.303*log10(E));

  %... N2+ cross section
  SIGIT(3)=0.0;
  if(E > 15.0)
    SIGIT(3)=1.42E-14*(1-9.0/E)^7.1*E^(-0.7);
  end
  if(SIGTMP < SIGIT(3))
    SIGIT(3)=SIGTMP;
  end
  %... This correction to convert units to cm**2. Keiffer and Dunn page 10
  SIGIT(3)=0.87972*SIGIT(3);

  %... O2+ cross section
  SIGIT(2)=0.0;
  if(E > 12.0)
    SIGIT(2)=1.08E-14*(1-7.0/E)^8.6*E^(-0.65);
  end
  if(SIGTMP < SIGIT(2))
    SIGIT(2)=SIGTMP;
  end
  %... This correction to convert units to cm**2. Keiffer and Dunn page 10
  SIGIT(2)=0.87972*SIGIT(2);

  %... O+ cross section from Brook et al. J. Phys. B. Vol 11 p 3115, 1978
  SIGIT(1)=0.0;
  if(E > 12.0)
    SIGIT(1)=7.33E-15*(1-2.0/E)^34.3*E^(-0.7);
  end

end

