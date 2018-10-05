function [ EPSTEP ] = EPSTEP( Y2, Y1, SC, HX, X )
%EPSTEP STEP FROM Y1 TO Y2
%
%        REAL FUNCTION EPSTEP ( Y2, Y1, SC, HX, X)
%---------------------------------------------- STEP FROM Y1 TO Y2      
  EPSTEP = Y1 + ( Y2 - Y1 ) * IRI2012.EPST ( X, SC, HX);

end

