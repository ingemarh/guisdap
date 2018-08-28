function [ FOUT ] = FOUT( XMODIP,XLATI,XLONGI,UT,FF0 )
%FOUT CALCULATES CRITICAL FREQUENCY FOF2/MHZ
%                     
%*************************************************************                  
%************* PEAK VALUES ELECTRON DENSITY ******************                  
%*************************************************************                  
%
%      real function FOUT(XMODIP,XLATI,XLONGI,UT,FF0)
%--------------------------------------------------------------
% CALCULATES CRITICAL FREQUENCY FOF2/MHZ USING SUBROUTINE GAMMA1.      
% XMODIP = MODIFIED DIP LATITUDE, XLATI = GEOG. LATITUDE, XLONGI=
% LONGITUDE (ALL IN DEG.), MONTH = MONTH, UT =  UNIVERSAL TIME 
% (DEC. HOURS), FF0 = ARRAY WITH RZ12-ADJUSTED CCIR/URSI COEFF.
% D.BILITZA,JULY 85.
%--------------------------------------------------------------

%      DIMENSION FF0(988)
%      INTEGER QF(9)
  persistent QF;
  if isempty(QF)
    QF = cast([11,11,8,4,1,0,0,0,0],IRI2012.float_t);
  end
  FOUT=IRI2012.GAMMA1(XMODIP,XLATI,XLONGI,UT,6,QF,length(QF), ...
    IRI2012.F2numJ,IRI2012.F2numI,IRI2012.F2numI*IRI2012.F2numJ,FF0);

end

