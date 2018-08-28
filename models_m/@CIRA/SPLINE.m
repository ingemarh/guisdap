function [Y,YI] = SPLINE(XS,YS,NM,MN,C,YD1,YD2,X)
  % Calculate spline coefficients
  [~,MD] = size(XS);
  [Y2,Y2dXS,Y2dYS,Y2dYD1,Y2dYD2] = CIRA.SPLINEM(XS,YS,NM,MN,C,YD1(1,1),YD2(1,1));
  % Calculate spline at X
  [Y3,YdX,YdXS,YdYS,YdYD1,YdYD2] = CIRA.SPLINTM(XS,YS,Y2,NM,MN,C,X(1,1),...
    Y2dXS,Y2dYS,Y2dYD1,Y2dYD2);
  Y = zeros(1,MD);
  Y(1,1) = Y3;
  for d=2:MD
    Y(1,d) = YdX*X(1,d)+YdYD1*YD1(1,d)+YdYD2*YD2(1,d);
    for K=NM:MN
      Y(1,d) = Y(1,d) + YdXS(K)*XS(K,d) + YdYS(K)*YS(K,d);
    end
  end
  if nargout > 1
    % integrate spline to X
    [YI3,YIdX,YIdXS,YIdYS,YIdYD1,YIdYD2] = CIRA.SPLINI(XS,YS,Y2,NM,MN,C,X(1,1),...
      Y2dXS,Y2dYS,Y2dYD1,Y2dYD2);
    YI = zeros(1,MD);
    YI(1,1) = YI3;
    for d=2:MD
      YI(1,d) = YIdX*X(1,d)+YIdYD1*YD1(1,d)+YIdYD2*YD2(1,d);
      for K=NM:MN
        YI(1,d) = YI(1,d) + YIdXS(K)*XS(K,d) + YIdYS(K)*YS(K,d);
      end
    end
  end
end
