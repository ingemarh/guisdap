function [ cgmgla ] = CGMGLA( context,clon )
%CGMGLA This function returns the geocentric latitude as a function of CGM
% longitude
%
%      real function cgmgla(clon)
%  *********************************************************************
%  This function returns the geocentric latitude as a function of CGM
%  longitude with the CGM latitude held in common block CGMGEO.
%  Essentially this function just calls the subroutine CORGEO.
%  *********************************************************************

%      logical cr360,cr0
%      common/cgmgeo/cclat,cr360,cr0,rh

  rr = context.rh;
  if clon > 360.
    clon = clon - 360.;
  end
  if clon < 0.
    clon = clon + 360.;
  end
  [geolat,~,~,~,~] = context.CORGEO(rr,context.clat,clon);
  cgmgla = geolat;

end

