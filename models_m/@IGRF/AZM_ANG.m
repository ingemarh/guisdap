function [ AZM_ANG ] = AZM_ANG( sla,slo,cla,pla,plo )
%AZM_ANG Computation of an angle between the north geographic meridian
%
%      real function AZM_ANG(sla,slo,cla,pla,plo)
%  *********************************************************************
%  Computation of an angle between the north geographic meridian and
%  direction to the North (South) CGM pole: positive azimuth is
%  measured East (West) from geographic meridian, i.e., the angle is
%  measured between the great-circle arc directions to the geographic
%  and CGM poles. In this case the geomagnetic field components in
%  XYZ (NEV) system can be converted into the CGM system in both
%  hemispheres as:
%                           XM = X cos(alf) + Y sin(alf)
%                           YM =-X sin(alf) + Y cos(alf)
%
%  Written by V. O. Papitashvili in mid-1980s; revised in February 1999
%
%  Ignore points which nearly coincide with the geographic or CGM poles
%  within 0.01 degree in latitudes; this also takes care if SLA or CLA
%  are dummy values (e.g., 999.99)
%  *********************************************************************

  if abs(sla) >= 89.99 || abs(cla) >= 89.99
    AZM_ANG = IGRF.BAD_ANGLE;
    return;
  end
  if pla*cla < 0
    fprintf(1,'WARNING - The CGM pole PLA = %6.2f and station CLAT = %6.2f are not in the same hemisphere: AZM_ANG is incorrect!\n', pla,cla);
  end

  am = (90. - abs(pla))*IGRF.UMR;
  if pla*sla >= 0
     cm = (90. - abs(sla))*IGRF.UMR;
  else
     cm = (90. + abs(sla))*IGRF.UMR;
  end
  if sla >= 0.
    bet = (plo - slo)*IGRF.UMR;
  else
    bet = (slo - plo)*IGRF.UMR;
  end
  sb = sin(bet);
  st = sin(cm)/tan(am) - cos(cm)*cos(bet);
  if sb == 0 && st == 0
    alfa = 0.0;
  else
    alfa = atan2(sb,st);
  end
  AZM_ANG = alfa/IGRF.UMR;

end

