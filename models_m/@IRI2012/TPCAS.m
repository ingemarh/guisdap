function [ TPASEA ] = TPCAS( MLTRAD,PF107,PF107M,XNDI,DNDI,PD,XNNI,DNNI,PN )
%TPCAS correction for season at fixed altitude
%
%       SUBROUTINE TPCAS(MLTRAD,PF107,PF107M,
%     &                  XNDI,DNDI,PD,XNNI,DNNI,PN,TPASEA)
%-------------------------------------------------------------------------------
%      correction for season at fixed altitude
%-------------------------------------------------------------------------------

%       REAL MLTRAD,PF107,PF107M,XNDI,DNDI,PD(3),XNNI,DNNI,PN(3),TPASEA
%       REAL TA,TM,TDC,TNC
  TA = IRI2012.TEDIFI(PF107,XNDI,DNDI,PD);
  TM = IRI2012.TEDIFI(PF107M,XNDI,DNDI,PD);
  TDC=TA-TM;
  TDC=max(TDC,-1250.);
  TDC=min(TDC,1250.);
  TA = IRI2012.TEDIFI(PF107,XNNI,DNNI,PN);
  TM = IRI2012.TEDIFI(PF107M,XNNI,DNNI,PN);
  TNC=TA-TM;
  TNC=max(TNC,-1250.);
  TNC=min(TNC,1250.);       
  %      harmonic interpolation for local time       
  TPASEA=(1.-cos(MLTRAD))/2.*(TDC-TNC)+TNC;

end

