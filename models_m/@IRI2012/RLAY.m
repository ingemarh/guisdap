function [ RLAY,dRLAY ] = RLAY( X, XM, SC, HX )
%RLAY RAWER  LAYER
%
%       REAL FUNCTION  RLAY ( X, XM, SC, HX )
% -------------------------------------------------------- RAWER  LAYER
  [Y1,dY1]  = IRI2012.EPTR( X , SC, HX );
  Y1M = IRI2012.EPTR( XM, SC, HX );
  Y2M = IRI2012.EPST( XM, SC, HX );
  RLAY = Y1 - Y1M - ( X - XM ) * Y2M / SC;
  dRLAY = dY1 - Y2M / SC;

end

