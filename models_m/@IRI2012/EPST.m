function [ EPST ] = EPST( X, SC, HX )
%EPST STEP
%
%        REAL FUNCTION EPST ( X, SC, HX )
% -------------------------------------------------------------- STEP

%        COMMON/ARGEXP/ARGMAX
  lim = SC*IRI2012.ARGMAX;
  if X < HX-lim || X > HX+lim
    if X > HX
      EPST = 1.;
    else
      EPST = 0.;
    end
  else
    D1 = ( X - HX ) / SC;
    EPST = 1. / ( 1. + exp( -D1 ));
  end


end

