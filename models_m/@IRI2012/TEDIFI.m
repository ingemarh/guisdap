function [ TEDIF ] = TEDIFI( F107IN,TEXN,TEDN,F107DF )
%TEDIFI interpolation for solar activity
%
%       SUBROUTINE TEDIFI(F107IN,TEXN,TEDN,F107DF,TEDIF)
%-------------------------------------------------------------------------------
%      interpolation for solar activity        
%-------------------------------------------------------------------------------

%       REAL F107IN,TEXN,TEDN,F107DF(3),TEDIF
%       REAL T0DNXN(3),T0DN(2),TDNXN(2)
%       REAL INTERP
  if (F107IN >= F107DF(1)) && (F107IN <= F107DF(3))
    T0DNXN = zeros(3,1);
    %T0DNXN(1)=0.;
    T0DNXN(2)=TEDN;
    T0DNXN(3)=TEXN;
    TEDIF=IRI2012.INTERP(3,2,T0DNXN,1,F107DF,F107IN);
  elseif F107IN < F107DF(1)
    T0DN = zeros(2,1);
    %T0DN(1)=0.;
    T0DN(2)=TEDN;
    TEDIF=IRI2012.INTERP(2,1,T0DN,1,F107DF,F107IN);
  else %if F107IN > F107DF(3)
    TDNXN = zeros(2,1);
    TDNXN(1)=TEDN;
    TDNXN(2)=TEXN;
    TEDIF=IRI2012.INTERP(2,1,TDNXN,1,F107DF(2:3),F107IN);
  end

end

