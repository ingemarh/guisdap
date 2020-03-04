function obox = orientedBox(points)
%ORIENTEDBOX Minimum-width oriented bounding box of a set of points
%
%   OBOX = orientedBox(PTS)
%   Computes the oriented bounding box of a set of points. Oriented box is
%   defined by a center, two dimensions (the length and the width), and the
%   orientation of the length axis. Orientation is counted in degrees, 
%   counter-clockwise.
%
%   Example
%     % Draw oriented bounding box of an ellipse
%     elli = [30 40 40 20 30];
%     pts = ellipseToPolygon(elli, 120);
%     obox = orientedBox(pts);
%     figure; hold on;
%     drawEllipse(elli);
%     drawOrientedBox(obox, 'm');
%
%   See also
%   drawOrientedBox, orientedBoxToPolygon
%

% ------
% Author: David Legland
% e-mail: david.legland@nantes.inra.fr
% Created: 2012-03-29,    using Matlab 7.9.0.529 (R2009b)
% Copyright 2012 INRA - Cepia Software Platform.


%% initialisations

% first, compute convex hull of the polygon
inds = convhull(points(:,1), points(:,2));
hull = points(inds, :);

% if first and last points are the same, remove the last one
if inds(1) == inds(end)
    hull = hull(1:end-1, :);
end

% compute convex hull centroid, that corresponds to approximate
% location of rectangle center
center = mean(hull, 1);
hull = bsxfun(@minus, hull, center);

% number of hull vertices
nV = size(hull, 1);

% default values
rotatedAngle = 0;
minWidth = inf;
minAngle = 0;

% avoid degenerated cases
if nV < 3
    return;
end

% indices of vertices in extreme y directions
[tmp, indA] = min(hull(:, 2)); %#ok<ASGLU>
[tmp, indB] = max(hull(:, 2)); %#ok<ASGLU>

caliperA = [ 1 0];    % Caliper A points along the positive x-axis
caliperB = [-1 0];    % Caliper B points along the negative x-axis


%% Find the direction with minimum width (rotating caliper algorithm)

while rotatedAngle < pi
    % compute the direction vectors corresponding to each edge
    indA2 = mod(indA, nV) + 1;
    vectorA = hull(indA2, :) - hull(indA, :);
    
    indB2 = mod(indB, nV) + 1;
    vectorB = hull(indB2, :) - hull(indB, :);
    
    % Determine the angle between each caliper and the next adjacent edge
    % in the polygon 
    angleA = vectorAngle(caliperA, vectorA);
    angleB = vectorAngle(caliperB, vectorB);
    
    % Determine the smallest of these angles
    angleIncrement = min(angleA, angleB);
    
    % Rotate the calipers by the smallest angle
    caliperA = rotateVector(caliperA, angleIncrement);
    caliperB = rotateVector(caliperB, angleIncrement);
    
    rotatedAngle = rotatedAngle + angleIncrement;
    
    % compute current width, and update opposite vertex
    if angleA < angleB
        line = createLine(hull(indA, :), hull(indA2, :));
        width = distancePointLine(hull(indB, :), line);
        indA = mod(indA, nV) + 1;
    
    else
        line = createLine(hull(indB, :), hull(indB2, :));
        width = distancePointLine(hull(indA, :), line);
        indB = mod(indB, nV) + 1;

    end
    
    % update minimum width and corresponding angle if needed
    if width < minWidth
        minWidth = width;
        minAngle = rotatedAngle;
    end
end


%% Compute box dimensions

% orientation of the main axis
theta = rad2deg(minAngle);

% pre-compute trigonometric functions
cot = cos(minAngle);
sit = sin(minAngle);

% elongation in direction of rectangle length
x = hull(:,1);
y = hull(:,2);
x2  =   x * cot + y * sit;
y2  = - x * sit + y * cot;

% compute extension along main directions
xmin = min(x2);    xmax = max(x2);
ymin = min(y2);    ymax = max(y2);

% position of the center with respect to the centroid compute before
dl = (xmax + xmin)/2;
dw = (ymax + ymin)/2;

% change  coordinate from rectangle to user-space
dx  = dl * cot - dw * sit;
dy  = dl * sit + dw * cot;

% coordinates of oriented box center
center = center + [dx dy];

% size of the rectangle
rectLength  = xmax - xmin;
rectWidth   = ymax - ymin;

% concatenate rectangle data
obox = [center rectLength rectWidth theta];



function alpha = vectorAngle(v1, varargin)
%VECTORANGLE Angle of a vector, or between 2 vectors

 
%% Initializations
 
% default values
v2 = [];
cutAngle = pi;
 
% process input arguments
while ~isempty(varargin)
    var = varargin{1};
    if isnumeric(var) && isscalar(var)
        % argument is normalization constant
        cutAngle = varargin{1};
        varargin(1) = [];
        
    elseif isnumeric(var) && size(var, 2) == 2
        % argument is second vector
        v2 = varargin{1};
        varargin(1) = [];
        
    elseif ischar(var) && length(varargin) >= 2
        % argument is option given as string + value
        if strcmpi(var, 'cutAngle')
            cutAngle = varargin{2};
            varargin(1:2) = [];
            
        else
            error(['Unknown option: ' var]);
        end
         
    else
        error('Unable to parse inputs');
    end
end
 
 
%% Case of one vector

% If only one vector is provided, computes its angle
if isempty(v2)
    % compute angle and format result in a 2*pi interval
    alpha = atan2(v1(:,2), v1(:,1));
     
    % normalize within a 2*pi interval
    alpha = normalizeAngle(alpha + 2*pi, cutAngle);
    
    return;
end
 
 
%% Case of two vectors
 
% compute angle of each vector
alpha1 = atan2(v1(:,2), v1(:,1));
alpha2 = atan2(v2(:,2), v2(:,1));
 
% difference
alpha = bsxfun(@minus, alpha2, alpha1);

% normalize within a 2*pi interval
alpha = normalizeAngle(alpha + 2*pi, cutAngle);



function alpha = normalizeAngle(alpha, varargin)
%NORMALIZEANGLE  Normalize an angle value within a 2*PI interval

center = pi;
if ~isempty(varargin)
    center = varargin{1};
end
 
alpha = mod(alpha-center+pi, 2*pi) + center-pi;



function vr = rotateVector(v, angle)
%ROTATEVECTOR Rotate a vector by a given angle

% precomputes angles
cot = cos(angle);
sit = sin(angle);

%compute rotated coordinates
vr = [cot * v(:,1) - sit * v(:,2) , sit * v(:,1) + cot * v(:,2)];



function line = createLine(varargin)
%CREATELINE Create a straight line from 2 points, or from other inputs
 
if length(varargin)==1
    % Only one input parameter. It can be :
    % - line angle
    % - array of four parameters
    % TODO : add control for arrays of lines.
    var = varargin{1};
     
    if size(var, 2)==4
        % 4 parameters of the line in a single array.
        line = var;
    elseif size(var, 2)==1
        % 1 parameter : angle of the line, going through origin.
        line = [zeros(size(var)) zeros(size(var)) cos(var) sin(var)];
    else
        error('wrong number of dimension for arg1 : can be 1 or 4');
    end
     
elseif length(varargin)==2    
    % 2 input parameters. They can be :
    % - line angle and signed distance to origin.
    % - 2 points, then 2 arrays of 1*2 double.
    v1 = varargin{1};
    v2 = varargin{2};
    if size(v1, 2)==1
        % first param is angle of line, and second param is signed distance
        % to origin.
        line = [v1.*cos(v2) v1.*sin(v2) -sin(v2) cos(v2)];
    else
        % first input parameter is first point, and second input is the
        % second point.
        line = [v1(:,1), v1(:,2), v2(:,1)-v1(:,1), v2(:,2)-v1(:,2)];    
    end
     
elseif length(varargin)==3
    % 3 input parameters :
    % first one is a point belonging to the line,
    % second and third ones are direction vector of the line (dx and dy).
    p = varargin{1};
    line = [p(:,1) p(:,2) varargin{2} varargin{3}];
    
elseif length(varargin)==4
    % 4 input parameters :
    % they are x0, y0 (point belongng to line) and dx, dy (direction vector
    % of the line).
    % All parameters should have the same size.
    line = [varargin{1} varargin{2} varargin{3} varargin{4}];
else
    error('Wrong number of arguments in ''createLine'' ');
end



function [dist pos] = distancePointLine(point, line)
%DISTANCEPOINTLINE Minimum distance between a point and a line
 
% direction vector of each line (row vectors)
vx = line(:, 3)';
vy = line(:, 4)';
 
% squared length of edges, with a check of valifity
delta = (vx .* vx + vy .* vy);
invalidEdges = delta < eps;
delta(invalidEdges) = 1; 
 
% difference of coordinates between point and edge first vertex
% (NP-by-NE arrays)
dx  = bsxfun(@minus, point(:, 1), line(:, 1)');
dy  = bsxfun(@minus, point(:, 2), line(:, 2)');
 
% compute position of points projected on the supporting line, by using
% normalised dot product (NP-by-NL array)
pos = bsxfun(@rdivide, bsxfun(@times, dx, vx) + bsxfun(@times, dy, vy), delta);
 
% ensure degenerated edges are correclty processed (consider the line
% origin as closest point)
pos(:, invalidEdges) = 0;
 
% compute distance between point and its projection on the edge
dist = hypot(bsxfun(@times, pos, vx) - dx, bsxfun(@times, pos, vy) - dy);