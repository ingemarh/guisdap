function [ UT ] = MLTUT( SLA,SLO,CLA,PLA,PLO )
%MLTUT Calculates the MLT midnight in UT hours
%      SUBROUTINE MLTUT(SLA,SLO,CLA,PLA,PLO,UT)
%  *********************************************************************
%  Calculates the MLT midnight in UT hours
%  Definition of the MLT midnight (MLTMN) here is different from the
%  approach described elsewhere. This definition does not take into
%  account the geomagnetic meridian of the subsolar point which causes
%  seasonal variations of the MLTMN in UT time. The latter approach is
%  perfectly applicable to the dipole or eccentric dipole magnetic
%  coordinates but it fails with the CGM coordinates because there are
%  forbidden areas near the geomagnetic equator where CGM coordinates
%  cannot be calculated by definition [e.g., Gustafsson et al., JATP,
%  54, 1609, 1992].
%  In this code the MLT midnight is defined as location of a given point
%  on (or above) the Earth's surface strictly behind the North (South)
%  CGM pole in such the Sun, the pole, and the point are lined up.
%  This approach was originally proposed and coded by Boris Belov
%  sometime in the beginning of 1980s; here it is slightly edited by
%  Vladimir Papitashvili in February 1999.
%  Ignore points which nearly coincide with the geographic or CGM poles
%  within 0.01 degree in latitudes; this also takes care if SLA or CLA
%  are dummy values (e.g., 999.99)
%  *********************************************************************

  persistent ANGLIM;
  if isempty(ANGLIM)
    ANGLIM = 0.0000001; % radians
  end

  if abs(SLA) >= 89.99 || abs(CLA) >= 89.99
    UT = 99.99;
    return;
  end

  if PLA*CLA < 0
    fprintf(1,'WARNING - The CGM pole PLA = %6.2f and station CLAT = %6.2f', PLA,CLA);
    fprintf(1,' are not in the same hemisphere: MLTMN is incorrect!\n');
  end

  %  Solve the spherical triangle

  QQ = PLO*IGRF.UMR;
  CFF = 90. - abs(PLA);
  CFF = CFF*IGRF.UMR;
  if CFF < ANGLIM
    CFF=ANGLIM;
  end

  if PLA*SLA >= 0
    CFT = 90. - abs(SLA);
  else
    CFT = 90. + abs(SLA);
  end

  CFT = CFT*IGRF.UMR;
  if CFT < ANGLIM
    CFT=ANGLIM;
  end

  QT = SLO*IGRF.UMR;
  A = sin(CFF)/sin(CFT);
  Y = A*sin(QQ) - sin(QT);
  X = cos(QT) - A*cos(QQ);
  UT = atan2(Y,X);

  if UT < 0.
    UT = UT + IGRF.twopi;
  end
  QQU = QQ + UT;
  QTU = QT + UT;
  BP = sin(CFF)*cos(QQU);
  BT = sin(CFT)*cos(QTU);
  UT = UT/IGRF.UMR;
  UT = UT/15.;
  if BP >= BT

    if UT < 12.
      UT = UT + 12.;
    end
    if UT > 12.
      UT = UT - 12.;
    end

  end

end

