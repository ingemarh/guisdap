function [ PROB ] = PROBS( ISW,ZLAM,LMAX,NNI )
%PROBS finding branching ratios
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE PROBS(ISW,PROB,ZLAM,LMAX,NNI)
%.... program for finding branching ratios (probabilities for various ion
%.... and molecular states) of o,o2,n2
%.... ---refs--- m. torr et al grl 1979 page 771, kirby et al atomic data
%.... and nuclear tables 1979 23,page 63

%      IMPLICIT NONE
%      INTEGER I,IS,ISW,J,L,LL,LMAX,NNI(3)
%      REAL YO(37,5),PROB(3,6,37),ZLAM(37),SUM
  persistent YO NI;
  if isempty(YO)
    NI = 37;
    %...... coefficients of o ionization cross sections from torr et al
    %..... table 2
    YO = transpose([.19,.486,.952,1.311,1.539,1.77,1.628,1.92,1.925,2.259 ...
       ,2.559,2.523,3.073,3.34,3.394,3.421,3.65,3.92,3.62,3.61,3.88,4.25 ...
       ,5.128,4.89,6.739,4.0,3.89,3.749,5.091,3.498,4.554,1.315,0.0,0.0,0.0,0.0,0.0; ...
       .206,.529,1.171,1.762,2.138,2.62,2.325,2.842,2.849,3.446,3.936 ...
       ,3.883,4.896,5.37,5.459,5.427,5.67,6.02,5.91,6.17,6.29,6.159 ...
       ,11.453,6.57,3.997,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0;...
       .134,.345,.768,1.144,1.363,1.63 ...
       ,1.488,1.92,1.925,2.173,2.558,2.422,2.986,3.22,3.274,3.211,3.27 ...
       ,3.15,3.494,3.62,3.23,2.956,0.664,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,...
       0.0,0.0,0.0,0.0,0.0;  .062,.163,.348,.508 ...
       ,.598,.71,.637,.691,.693,.815,.787,.859,.541,0.0,0.0,0.0,0.0,0.0,...
       0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,...
       0.0,0.0; .049,.13 ...
       ,.278,.366,.412,.35,.383,.307,.308,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,...
       0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,...
       0.0,0.0,0.0]);
  end
  PROB = zeros(3,6,NI);
  %
  %....... production of o states from torr et al table 2 (yo array)
  %....... need to reverse order of yo to correspond with lambda
  for L=1:LMAX
    LL=LMAX+1-L;
    SUM=YO(LL,1)+YO(LL,2)+YO(LL,3)+YO(LL,4)+YO(LL,5);
    for I=1:5
      PROB(1,I,L)=0.0;
      if SUM ~= 0.0
        PROB(1,I,L)=YO(LL,I)/SUM;
      end
    end
  end
  %
  %....... call separate subroutines for o2 and n2 probabilities
  for L=1:LMAX
    PROB = IRI2012.PROBO2(1,L,ZLAM(L),PROB,NNI(2));
    PROB = IRI2012.PROBN2(1,L,ZLAM(L),PROB,NNI(3));
  end

  if ISW == 0 || context.KONSOL <= 0
    return;
  end
  fprintf(context.KONSOL,'     Photoionization branching ratios for O, O2, N2   Lam    4S   2D   2P   4P   2P*   -   X2   a+A  b4   B2   dis   -  X2   A2   B2   C2   F2   2s');
  for L=1:LMAX
    for J=1:6
      for IS=1:3
        fprintf(context.KONSOL,'%8.2f %5.2f', ZLAM(L),PROB(IS,J,L));
      end
    end
  end

end

