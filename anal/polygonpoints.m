function [plonlat,PointInPol] = polygonpoints(lonlat,image)

if nargin < 2
    image = [];
end

lon = lonlat(isfinite(lonlat(:,1)),1);
lat = lonlat(isfinite(lonlat(:,2)),2);  % assuming

toldeg = 1e-4;
tol = toldeg/max(Vdate(1,:)*24*3600);       % tolerence = 0,01 s
lonlat_unique = uniquetol([lon,lat],1e-4,'ByRows',true);    % with a tolerance of 1e-4 degrees
nunique = length(lonlat_unique(:,1));

% if lon or lat is unique but not the other, calculate max 'distance'
if nunique > 2 && length(unique(lonlat_unique(:,1))) == 1
    maxdiff = max(lonlat_unique(:,2)) - min(lonlat_unique(:,2));
    lon = unique(lonlat_unique(:,1))*ones(2,1);
    lat = [min(lonlat_unique(:,2));max(lonlat_unique(:,2))]; 
elseif nunique > 2 && length(unique(lonlat_unique(:,2))) == 1
    maxdiff = max(lonlat_unique(:,1)) - min(lonlat_unique(:,1));
    lat = unique(lonlat_unique(:,2))*ones(2,1);
    lon = [min(lonlat_unique(:,1));max(lonlat_unique(:,1))]; 
end

if nunique == 1
    convarea = 0;
elseif nunique == 2
    convarea = sqrt(diff(lonlat_unique(:,1))^2 + diff(lonlat_unique(:,2))^2);   % 'distance'!!!
elseif exist('maxdiff','var')
    convarea = maxdiff;
else
    conv = convhull([lon,lat]);
    convarea = polyarea(lon(conv),lat(conv));
end

PointInPol = [];
if convarea < 10e-4
    [plon, plat] = deal(mean(lon),mean(lat));
    plonlat = [{num2str(plon)} {num2str(plat)}];
    PointInPol = [];
else
    if nunique == 2
        plon = lonlat_unique(:,1);
        plat = lonlat_unique(:,2);     
    elseif exist('maxdiff','var')
        plon = lon;
        plat = lat;
    else
        [plon,plat,c] = orientedPolygon([lon lat],image);
        if diff([max(plon) min(plon)])>180
            PointInPol = [{num2str(c(1))} {num2str(c(2))}];
        end
    end
    for iii = 1:length(plon)
        plonlat(iii,:) = [{num2str(plon(iii))} {num2str(plat(iii))}];
    end
end  

end