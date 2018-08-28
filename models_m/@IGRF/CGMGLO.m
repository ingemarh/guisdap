function [ cgmglo ] = CGMGLO( context,clon )
%CGMGLO Same as the function CGMGLA but this returns the geocentric longitude
%
%      real function CGMGLO(clon)
% *********************************************************************
%  Same as the function CGMGLA but this returns the geocentric
%  longitude. If cr360 is true, geolon+360 deg is returned when geolon
%  is less than 90 deg. If cr0 is true, geolon-360 deg is returned
%  when geolon is larger than 270 degrees.
% *********************************************************************

%      logical cr360,cr0
%
%      common/cgmgeo/cclat,cr360,cr0,rh

  rr = context.rh;
  if clon > 360.
    clon = clon - 360.;
  end
  if clon < 0.
    clon = clon + 360.;
  end
  clon0 = clon;
  while clon > clon0-360.0;
    [geolat,geolon,~,~,~] = context.CORGEO(rr,context.clat,clon);

    %  Geographic longitude geolon could be any number (e.g., discontinued)
    %  when geolat is the geographic pole

    if abs(geolat) >= 89.99
      clon = clon - 0.01;
    else
      break;
    end
  end
  if context.cr360 && (geolon <= 90.)
    cgmglo = geolon + 360.;
  else
    if context.cr0 && (geolon >= 270.)
      cgmglo = geolon - 360.;
    else
      cgmglo = geolon;
    end
  end


end

