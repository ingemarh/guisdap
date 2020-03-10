function [plonlat,PointInPol] = polygonpoints(lonlat,image)

if nargin < 2
    image = [];
end
    
lonlat_unique = unique([lonlat(:,1),lonlat(:,2)],'rows');
nunique = length(lonlat_unique(:,1));

if nunique == 1
    convarea = 0;
elseif nunique == 2
    convarea = sqrt(diff(lonlat_unique(:,1))^2 + diff(lonlat_unique(:,2))^2);   % distance!!!
else
    conv = convhull([lonlat(:,1),lonlat(:,2)]);
    convarea = polyarea(lonlat(conv,1),lonlat(conv,2));
end

if convarea < 10e-4
    [plon, plat] = deal(mean(lonlat(:,1)),mean(lonlat(:,2)));
    plonlat = [plon plat];
else
    if nunique == 2
        plon = lonlat_unique(:,1);
        plat = lonlat_unique(:,2);
    else
        [plon,plat,c] = orientedPolygon([lonlat(:,1) lonlat(:,2)],image);
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