function [ PROB ] = PROBN2( ISW,L,ZLAM,PROB,JPTS )
%PROBN2 N2 probabilities
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%         SUBROUTINE PROBN2(ISW,L,ZLAM,PROB,JPTS)
%...... the n2 probabilities are taken from kirby et al tables b and c
%...... the yield of n+ is determined first then the remaining portion
%...... of the cross section is distributed amongst the n2+ ion states
%...... (x,a,b). the dissociation yield is divided between the 3 higher
%...... energy states according to wight et al. j.phys. b, 1976
%...... the 2 other states of kirby et al are not included

%      IMPLICIT NONE
%      INTEGER I,IPTS,ISW,J,JPTS,L
%      REAL A(6),B(6),X(14),Y(14,6),PROB(3,6,37),SUM,YIELD,YLAM,ZLAM
  persistent IPTS X Y;
  if isempty(IPTS)
    IPTS = 14;
    X = [50.,210.,240.,280.,300.,332.,428.,500.,600.,660.,660.01, ...
         720.,747.,796.];
    Y = transpose([ ...
       .32,.32,.32,.32,.32,.30,.46,.404,.308,.308,.308,.42,1.,1.; ...
       .55,.55,.55,.55,.55,.52,.46,.506,.589,.589,.692,.58,.0,.0; ...
       .13,.13,.13,.13,.13,.12,.08,.090,.103,.103,.000,.00,.0,.0; ...
       .00,.00,.00,.05,.10,.15,.83,1.00,.000,.000,.000,.00,.0,.0; ...
       .00,.00,.00,.30,.40,.79,.17,.000,.000,.000,.000,.00,.0,.0; ...
       1.0,1.0,1.0,.65,.50,.06,.00,.000,.000,.000,.000,.00,.0,.0]);
  end
  A = zeros(JPTS,1);
  B = zeros(JPTS,1);
  %PROB = zeros(3,JPTS,37);
  %
  %...... if zlam is too big set equal to x(max)
  YLAM=ZLAM;
  %.. Prevent divide by zero
  if(ZLAM > X(IPTS))
    YLAM=X(IPTS)-1;
  end
  if(ZLAM < X(1))
    YLAM=X(1)+1;
  end

  %YIELD=0.0;
  %...... determine yield of n+, and store in prob array
  YIELD = IRI2012.YLDISS(1,YLAM);

   for I=1:IPTS
    % kjh 6/22/92   NOTE:  I realize the following statement is strange
    %   looking, but its purpose is to prevent the CRAY compiler from
    %   vectorizing this loop.  (Which it does incorrectly).
    %if(I == 25)
    %  fprintf(1,' ');
    %end
    if (YLAM > X(I) && YLAM <= X(I+1))
      break;
    end
  end
  SUM=0.0;
  %...... fit straight line between points
  for J=1:JPTS
    A(J)=(Y(I+1,J)-Y(I,J))/(X(I+1)-X(I));
    B(J)=Y(I,J)-A(J)*X(I);
  end
  %...... determine probabilities of n2+ states
  for J=1:JPTS
    if(J <= 3)
      PROB(3,J,L)=(A(J)*YLAM+B(J))*(1-YIELD);
    end
    if(J > 3)
      PROB(3,J,L)=(A(J)*YLAM+B(J))*YIELD;
    end
    SUM=SUM+PROB(3,J,L);
  end

  if(SUM == 0.0)
    return;
  end
  %....... normalise probabilities
  for J=1:JPTS
    PROB(3,J,L)=PROB(3,J,L)/SUM;
  end


end

