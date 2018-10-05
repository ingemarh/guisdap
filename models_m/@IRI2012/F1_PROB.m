function [ f1prob, f1probl ] = F1_PROB( sza,glat,rz12 )
%F1_PROB Occurrence probability of F1 layer
%
%        subroutine f1_prob (sza,glat,rz12,f1prob,f1probl)
%--------------------------------------------------------------------------
% Occurrence probability of F1 layer after Scotto et al., Advances in
% Space Research, Volume 20, Number 9, 1773-1775, 1997.
%
% Input: 	sza		solar zenith angle in degrees 
% 			glat	geomagnetic latitude in degrees
%			rz12	12-month running mean of sunspot number
% Output: 	f1prob	F1 occurrence probability without L-condition cases 
% 			f1probl	F1 occurrence probability with L-condition cases
%--------------------------------------------------------------------------

%        common /const/umr

  xarg = 0.5 + 0.5 * cos(sza*IRI2012.UMR);
  a = 2.98 + 0.0854 * rz12;
  b = 0.0107 - 0.0022 * rz12;
  c = -0.000256 + 0.0000147 * rz12;
  gamma = a + ( b + c * glat) * glat;
  f1pr = xarg ^ gamma;
  if f1pr < 1.e-3
    f1pr=0.0;
  end
  f1prob=f1pr;
  f1prl = xarg ^ 2.36;
  if f1prl < 1.e-3
    f1prl=0.0;
  end
  f1probl=f1prl;

end

