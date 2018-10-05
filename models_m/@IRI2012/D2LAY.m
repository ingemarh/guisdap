function [ D2LAY ] = D2LAY( X, XM, SC, HX )
%D2LAY second derivative of LAY w.r.t. X
%
%        REAL FUNCTION D2LAY ( X, XM, SC, HX )
% ---------------------------------------------------------- d2LAY/dX2
  D2LAY = IRI2012.EPLA(X,SC,HX) /  (SC * SC);

end

