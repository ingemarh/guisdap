function [ SCHUPR,SCHUHT ] = SCHUMN( context,J,Z,ZO2,COLUMN,SCHUPR,SCHUHT )
%SCHUMN production of o(1d)
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE SCHUMN(J,Z,ZO2,COLUMN,SCHUPR,SCHUHT)
%......... production of o(1d) by schumann-runge bands
%......... The fluxes are from Torr et al. GRL 1980 p6063. Scaling is
%......... done using UVFAC which may be set according to F10.7 cm flux
%......... may be done in FACEUV

%      IMPLICIT NONE
%      INTEGER J,JTI,LMAX,LSR
%      REAL Z,ZO2,SCHUPR,SCHUHT,HSRX,FLD,EUV,SRXSCT,UVFAC
%      COMMON/SOL/UVFAC(59),EUV
%      REAL COLUMN(3),SRFLUX(8),SRXS(8),SRLAM(8)
  persistent SRFLUX SRXS SRLAM;
  if isempty(SRFLUX)
    SRFLUX = [2.4,1.4,.63,.44,.33,.17,.12,.053];
    SRXS = [.5,1.5,3.4,6,10,13,15,12];
    SRLAM = [1725,1675,1625,1575,1525,1475,1425,1375];
  end
  %
  %........ lmax=# of lambdas in sub. primpr: schuht=heating: schupr=o(1d) prod
  LMAX=37;

  for LSR=1:8
    %......... photoabsorption cross section
    SRXSCT=1.0E-18*SRXS(LSR);
    HSRX=SRXSCT*COLUMN(2);
    if HSRX > 70
      HSRX=70;
    end
    %........ attentuated solar flux
    FLD=context.UVFAC(LMAX+LSR)*1.E+11*SRFLUX(LSR)*exp(-HSRX);
    %............ neutral heating SCHUHT and photodissociation rate SCHUPR
    SCHUHT=SCHUHT+1.24E+4*(FLD*SRXSCT)*ZO2/SRLAM(LSR);
    SCHUPR=SCHUPR+FLD*SRXSCT;
    %if JTI == 0
    %  fprintf(context.KONSOL,'  ,%5d,%9.1f,%9.1f,%9.1f,%9.1f,%9.1f,%9.1f', LSR,SRXSCT,FLD,SCHUPR,COLUMN(2),FLD,context.UVFAC(LMAX+LSR));
    %end
  end

  SCHUPR=ZO2*SCHUPR;
  %      JTI=1;


end

