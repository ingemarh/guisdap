function [ PROB ] = PROBO2( ISW,L,ZLAM,PROB,JPTS )
%PROBO2 o2 branching ratios
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%         SUBROUTINE PROBO2(ISW,L,ZLAM,PROB,JPTS)
%....... o2 branching ratios are taken from kirby et al table d
%....... columns 4 & 9 are combined. columns 5,6,7&8 are combined

%      IMPLICIT NONE
%      INTEGER ISW,I,IPTS,J,JPTS,L
%      REAL A(5),B(5),X(20),Y(20,5),PROB(3,6,37),SUM,YLAM,ZLAM
  persistent IPTS X Y;
  if isempty(IPTS)
    IPTS = 20;
    X = [304.,323.,454.,461.,504.,537.,556.,573.,584.,598. ...
        ,610.,637.,645.,662.,684.,704.,720.,737.,774.,1026.];
    Y = transpose([.365,.374,.432,.435,.384,.345,.356,.365,.306,.23,.235,.245, ...
       .34,.27,.482,.675,.565,.565,1.,1.;.205,.21,.243,.245,.27,.29, ...
       .23,.27,.33,.295,.385,.35,.305,.385,.518,.325,.435,.435,.0,.0; ...
       .125,.124,.12,.12,.126,.13,.225,.216,.21,.375,.305,.37,.33,.345, ...
       .0,.0,.0,.0,.0,.0;.055,.167,.11,.105,.194,.234,.189,.149,.155,.103,.075,.036 ...
       ,.025,.0,.0,.0,.0,.0,.0,0.;.25,.125,.095,.95,.026,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,.0,0.]);
  end
  A = zeros(JPTS,1);
  B = zeros(JPTS,1);
  %
  %...... if zlam is too big set equal to x(max)
  YLAM=ZLAM;
  %...... if zlam is outside range of data values set equal to max or min
  if(ZLAM > X(IPTS))
    YLAM=X(IPTS);
  end
  if(ZLAM <= X(1))
    YLAM=X(1)+1.E-3;
  end

  for I=1:IPTS
    % kjh 6/22/92   NOTE:  I realize the following statement is strange
    %   looking, but its purpose is to prevent the CRAY compiler from
    %   vectorizing this loop.  (Which it does incorrectly).
    %if(I == 25)
      %fprintf(1,' ');
    %end
    if(YLAM > X(I) && YLAM <= X(I+1))
      break;
    end
  end
  SUM=0.0;

  for J=1:JPTS
    A(J)=(Y(I+1,J)-Y(I,J))/(X(I+1)-X(I));
    B(J)=Y(I,J)-A(J)*X(I);
    SUM=SUM+A(J)*YLAM+B(J);
  end

  for J=1:JPTS
    PROB(2,J,L)=(A(J)*YLAM+B(J))/SUM;
  end

end

