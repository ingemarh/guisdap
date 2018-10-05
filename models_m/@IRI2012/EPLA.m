function [ EPLA ] = EPLA( X, SC, HX )
%EPLA PEAK
%
%        REAL FUNCTION EPLA ( X, SC, HX )
% ------------------------------------------------------------ PEAK 

%        COMMON/ARGEXP/ARGMAX
  D1 = ( X - HX ) / SC;
  if abs(D1) >= IRI2012.ARGMAX
    EPLA = 0;
  else
    D0 = exp ( D1 );
    D2 = 1. + D0;
    EPLA = D0 / ( D2 * D2 );
  end

end

