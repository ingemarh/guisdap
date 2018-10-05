function [ f1_c1 ] = F1_C1( xmodip,hour,suxnon,saxnon )
%F1_C1 F1 layer shape parameter C1
%
%	real function f1_c1(xmodip,hour,suxnon,saxnon)
% F1 layer shape parameter C1 after Reinisch and Huang, Advances in
% Space Research, Volume 25, Number 1, 81-88, 2000.

%        common	/const/umr
        %pi = IRI2012.UMR * 180.;
	
  ABSMDP=abs(xmodip);
  if ABSMDP >= 18.
    DELA=1.0+exp(-(ABSMDP-30.0)/10.0);
  else
    DELA=4.32;
  end

  c1old = 0.09 + 0.11/DELA;
  if suxnon == saxnon
    c1 = 2.5 * c1old;
  else
    c1 = 2.5*c1old*cos((hour-12.)/(suxnon-saxnon)*IRI2012.pi);
  end
  if c1 < 0.0
    c1=0.0;
  end
  f1_c1=c1;

end

