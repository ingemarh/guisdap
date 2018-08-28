function [ INVDPC ] = INVDPC( FL,DIMO,B0,DIPL,DTOR )
%INVDPC calculation of INVDIP from FL, DIMO, BO, and DIPL
%
%      REAL FUNCTION INVDPC(FL,DIMO,B0,DIPL,DTOR)
%---------------------------------------------------------------------------
%      calculation of INVDIP from FL, DIMO, BO, and DIPL
%      invariant latitude calculated by highly
%      accurate polynomial expansion
%---------------------------------------------------------------------------

%      REAL FL,DIMO,B0,DIPL
%	DOUBLE PRECISION B(8),A
%      REAL DTOR,ASA,INVL,RINVL,RDIPL,ALFA,BETA
  persistent B;
  if isempty(B)
    B = [0.00183142,-0.00105877,0.00082777,-0.00308824, ...
         -0.01314096,-0.04686632,-0.1984259,1.259921,0.0];
  end
  A=(DIMO/B0)^(1.0/3.0)/FL;
  ASA=polyval(B,A);
  if ASA > 1.0
    ASA=1.0;
  end
  %      invariant latitude (absolute value)
  RINVL=acos(sqrt(ASA));
  INVL=RINVL/DTOR;
  RDIPL=DIPL*DTOR;
  ALFA=sin(abs(RDIPL))^3;
  BETA=cos(RINVL)^3;
  if DIPL >= 0
    INVDPC=(ALFA*INVL+BETA*DIPL)/(ALFA+BETA);
  else
    INVDPC=(-ALFA*INVL+BETA*DIPL)/(ALFA+BETA);
  end

end

