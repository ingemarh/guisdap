function [ EPTR, dEPTR ] = EPTR( X, SC, HX )
%EPTR TRANSITION
%
%        REAL FUNCTION EPTR ( X, SC, HX )
% --------------------------------------------------------- TRANSITION

%        COMMON/ARGEXP/ARGMAX
  if SC == 0.0 || abs( X - HX ) >= abs(SC)*IRI2012.ARGMAX
    if SC == 0.0 && X == HX
      EPTR = 0.0; % undefined 0/0
      dEPTR = 1.0 / SC; % infinity
    elseif (SC >= 0.0 && X > HX) || (SC <= 0.0 && X < HX)
      EPTR = ( X - HX ) / SC;
      dEPTR = 1.0 / SC;
    else
      EPTR = 0.0;
      dEPTR = 0.0;
    end
  else
    D1 = ( X - HX ) / SC;
    EPTR = log ( 1. + exp( D1 ) );
    dEPTR = exp( D1 ) / SC / ( 1. + exp( D1 ) );
  end
end

