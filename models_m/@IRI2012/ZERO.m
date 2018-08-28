function [ ZERO ] = ZERO( context, DELTA )
%ZERO FOR A PEAK AT X0 THE FUNCTION ZERO HAS TO BE EQUAL TO 0.
%        REAL FUNCTION ZERO(DELTA)
% FOR A PEAK AT X0 THE FUNCTION ZERO HAS TO BE EQUAL TO 0.

%        COMMON  /BLO10/         BETA,ETA,DEL,ZETA
%     &          /ARGEXP/        ARGMAX

  arg1=DELTA/100.;
  if abs(arg1) < IRI2012.ARGMAX
    z1=1./(1.+exp(arg1));
  elseif arg1 < 0
    z1=1.;
  else
    z1=0.;
  end
  arg1=(DELTA+94.5)/context.BETA;
  if abs(arg1) < IRI2012.ARGMAX
    z2=1./(1.+exp(arg1));
  elseif arg1 < 0
    z2=1.;
  else
    z2=0.;
  end
  ZERO=context.ZETA*(1.-z1) - context.ETA*z2;

end

