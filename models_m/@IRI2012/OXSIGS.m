function [ SIGEX,SIGEXT ] = OXSIGS( E )
%OXSIGS Inelastic cross sections for electron impact on atomic oxygen
%:::::::::::::::::::::: OXSIGS :::::::::::::::::::::::::::::::::::::
%      SUBROUTINE OXSIGS(E,SIGEX,SIGEXT)
%....... Inelastic cross sections for electron impact on atomic oxygen
%....... E=electron energy, SIGEX(22)=array of partial cross sections,
%....... SIGEXT=total excitation cross section, and S

%      DIMENSION SIGEX(22),SO1D(7)
  persistent SO1D;
  if isempty(SO1D)
    %..- CROSS SECTION FOR O(1D) - New Doering cross section from JGR
    %..- p19531, 1992. Increases production by a factor of 1.13
    SO1D = [0.0,0.0,15.0,30.0,44.0,54.0,38.0];
  end
  SIGEX = zeros(1,22);
  %..      IF(E.GT.6.5) SIGEX(1)=3.77E-15*(1-2.0/E)**4.25/E**1.7
  %..      IF(E.LE.7.3) SIGEX(1)=SO1D(NINT(E))*1.0E-18
  %
  %..... Old cross section of Henry
  if(E > 1.96)
    SIGEX(1)=4E-16*(1-1.96/E)^2/E;
  end
  %........ O(1S) cross section: may be double Shyn et al. JGR 1986, 13751
  if(E > 4.17)
    SIGEX(2)=6.54E-17*(1-sqrt(4.17/E))/E;
  end
  %....... 1304, 1027 A, Zipf and Erdman JGR 1985, 11088 include cascade.
  %....... Direct excitation is half for  1304 (Vaughan and Doering,
  %........ JGR 1986, 13755 and 1987 in press)
  if(E >= 10)
    SIGEX(3)=67.6E-17*(E-10)/E^2;
  end
  %....... 989 cross section from Doering 1987 (1/2 of Zipf)
  if(E >= 14)
    SIGEX(4)=7.0E-17*(1-14/E)/sqrt(E);
  end
  SIGEX(5)=0.38*SIGEX(4);
  %....... O(5S) 1356 A Stone And Zipf Corrected By Zipf And Erdman 1985
  %..- reparameterized 1 May 92 using FITXS.FOR (PGR)
  if(E > 10.0)
    SIGEX(6)=4.867E-12*(1.0-9.0/E)^2.67/ E^4.0;
  end
  SIGEXT=SIGEX(1)+(SIGEX(2)+SIGEX(3)+SIGEX(4)+SIGEX(5)+SIGEX(6));

end

