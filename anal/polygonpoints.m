function [plonlat,PointInPol] = polygonpoints(lonlat,image)

if nargin < 2
    image = [];
end

lon = lonlat(isfinite(lonlat(:,1)),1);
lat = lonlat(isfinite(lonlat(:,2)),2);  % assuming

lonlat_unique = uniquetol([lon,lat],1e-4,'ByRows',true);    % with a tolerance of 1e-4 degrees
nunique = length(lonlat_unique(:,1));

if nunique == 1
    convarea = 0;
elseif nunique == 2
    convarea = sqrt(diff(lonlat_unique(:,1))^2 + diff(lonlat_unique(:,2))^2);   % distance!!!
else
    conv = convhull([lon,lat]);
    convarea = polyarea(lon(conv),lat(conv));
end

if convarea < 10e-4
    [plon, plat] = deal(mean(lon),mean(lat));
    plonlat = [{num2str(plon)} {num2str(plat)}];
    PointInPol = [];
else
    if nunique == 2
        plon = lonlat_unique(:,1);
        plat = lonlat_unique(:,2);
        PointInPol = [];
    else
        [plon,plat,c] = orientedPolygon([lon lat],image);
        if diff([max(plon) min(plon)])>180
            PointInPol = [{num2str(c(1))} {num2str(c(2))}];
        else
            PointInPol = [];
        end
    end
    for iii = 1:length(plon)
        plonlat(iii,:) = [{num2str(plon(iii))} {num2str(plat(iii))}];
    end
end  

end