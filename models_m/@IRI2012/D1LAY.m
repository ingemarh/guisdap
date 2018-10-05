function [ D1LAY ] = D1LAY( X, XM, SC, HX )
%D1LAY derivative of LAY w.r.t. X
%
%        REAL FUNCTION D1LAY ( X, XM, SC, HX )
% ------------------------------------------------------------ dLAY/dX
  D1LAY = ( IRI2012.EPST(X,SC,HX) - IRI2012.EPST(XM,SC,HX) ) /  SC;

end

