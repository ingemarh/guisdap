function [ fmodip,context ] = FMODIP( context,xlat )
%FMODIP modified magnetic dip
% 
%		function fmodip(xlat)

%		common/findRLAT/xlong,year
%		
  [~,~,~,fmodip,context.igrfctx] = context.igrfctx.IGRF_DIP(xlat,context.FLON, ...
    context.RYEAR,300.);

end

