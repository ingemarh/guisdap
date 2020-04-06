function [tx, ty, c] = orientedPolygon(points,imag)

obox = orientedBox(points);
c = obox(1:2);
[tx, ty] = orientedBoxtoPolygon(obox);

if nargin < 2
    imag = [];
end

if imag
    cc = convhull([tx, ty]); 
    figure
    plot(points(:,1),points(:,2),'.b',tx(cc),ty(cc),'--om',c(1),c(2),'xm')
    axis equal
end

function obox = orientedBox(points)
%ORIENTEDBOX Minimum-width oriented bounding box of a set of points.
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

function line = createLine(varargin)
%CREATELINE Create a straight line from 2 points, or from other inputs.
%
%   Line is represented in a parametric form : [x0 y0 dx dy]
%   x = x0 + t*dx
%   y = y0 + t*dy;
%
%
%   L = createLine(p1, p2);
%   Returns the line going through the two given points.
%   
%   L = createLine(x0, y0, dx, dy);
%   Returns the line going through point (x0, y0) and with direction
%   vector(dx, dy).
%
%   L = createLine(LINE);
%   where LINE is an array of 4 values, creates the line going through the
%   point (LINE(1) LINE(2)), and with direction given by vector (LINE(3)
%   LINE(4)). 
%   
%   L = createLine(THETA);
%   Create a polar line originated at (0,0) and with angle THETA.
%
%   L = createLine(p, THETA);
%   Create a polar line originated at p and with angle THETA.
%
%   L = createLine(RHO, THETA);
%   Create a polar line with normal theta, and with min distance to origin
%   equal to rho. rho can be negative, in this case, the line is the same
%   as with CREATELINE(-rho, theta+pi), but the orientation is different.
%
%
%   Note: in all cases, parameters can be vertical arrays of the same
%   dimension. The result is then an array of lines, of dimensions [N*4].
%
%
%   See also:
%   lines2d, createEdge, createRay
%
%   ---------
%   author : David Legland 
%   INRA - TPV URPOI - BIA IMASTE
%   created the 31/10/2003.
%

%   HISTORY :
%   18/02/2004 : add more possibilities to create lines (4 parameters,
%      all param in a single tab, and point + dx + dy.
%      Also add support for creation of arrays of lines.

%   NOTE : A line can also be represented with a 1*5 array : 
%   [x0 y0 dx dy t].
%   whith 't' being one of the following : 
%   - t=0 : line is a singleton (x0,y0)
%   - t=1 : line is an edge segment, between points (x0,y0) and (x0+dx,
%   y0+dy).
%   - t=Inf : line is a Ray, originated from (x0,y0) and going to infinity
%   in the direction(dx,dy).
%   - t=-Inf : line is a Ray, originated from (x0,y0) and going to infinity
%   in the direction(-dx,-dy).
%   - t=NaN : line is a real straight line, and contains all points
%   verifying the above equation.
%   This seems us a convenient way to represent uniformly all kind of lines
%   (including edges, rays, and even point).
%

%   NOTE2 : Any line object can be represented using a 1x6 array :
%   [x0 y0 dx dy t0 t1]
%   the first 4 parameters define the supporting line,
%   t0 represent the position of the first point on the line, 
%   and t1 the position of the last point.
%   * for edges : t0 = 0, and t1=1
%   * for straight lines : t0 = -inf, t1=inf
%   * for rays : t0=0, t1=inf (or t0=-inf,t1=0 for inverted ray).
%   I propose to call these objects 'lineArc'

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
    if size(v1, 2)==2 && size(v2, 2)==1
        % first param is point, and second param is angle of line
        line = [v1(:,1), v1(:,2) cos(v2) sin(v2)];
    elseif size(v1, 2)==1
        % first param is angle of line, and second param is signed distance
        % to origin.
        line = [v1.*cos(v2) v1.*sin(v2) -sin(v2) cos(v2)];
    elseif size(v1, 2)==3 || size(v2, 2)==3
        error('The 1st or 2nd input argument has 3 columns. You may want to try createLine3d.');
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

function [dist, pos] = distancePointLine(point, line)
%DISTANCEPOINTLINE Minimum distance between a point and a line.
%
%   D = distancePointLine(POINT, LINE)
%   Return the euclidean distance between line LINE and point POINT. 
%
%   LINE has the form: [x0 y0 dx dy], and POINT is [x y].
%
%   If LINE is N-by-4 array, result is N-by-1 array computes for each line.
%
%   If POINT is N-by-2, then result is computed for each point.
%
%   If both POINT and LINE are array, result is computed for each couple of
%   point and line, and is returned in a NP-by-NL array, where NP is the
%   number of points, and NL is the number of lines.
%
%
%   See also:
%   lines2d, points2d, distancePoints, distancePointEdge
%
   
% ------
% Author: David Legland
% e-mail: david.legland@nantes.inra.fr
% Created: 2005-06-24
% Copyright 2016 INRA - BIA-BIBS.

%   HISTORY:
%   2012-10-24 rewrite using bsxfun

% direction vector of each line (row vectors)
vx = line(:, 3)';
vy = line(:, 4)';

% squared norm of direction vectors, with a check of validity
delta = (vx .* vx + vy .* vy);
invalidEdges = delta < eps;
delta(invalidEdges) = 1; 

% difference of coordinates between point and line origins
% (NP-by-NE arrays)
dx  = bsxfun(@minus, point(:, 1), line(:, 1)');
dy  = bsxfun(@minus, point(:, 2), line(:, 2)');

% compute position of points projected on the line, by using normalised dot
% product 
% (result is a NP-by-NL array) 
pos = bsxfun(@rdivide, bsxfun(@times, dx, vx) + bsxfun(@times, dy, vy), delta);

% ensure degenerated lines are correclty processed (consider the line
% origin as closest point)
pos(:, invalidEdges) = 0;

% compute distance between point and its projection on the line
dist = hypot(bsxfun(@times, pos, vx) - dx, bsxfun(@times, pos, vy) - dy);


% if size(line, 1)==1 && size(point, 1)>1
%     line = repmat(line, [size(point, 1) 1]);
% end
% 
% if size(point, 1)==1 && size(line, 1)>1
%     point = repmat(point, [size(line, 1) 1]);
% end
% 
% dx = line(:, 3);
% dy = line(:, 4);
% 
% % compute position of points projected on line
% tp = ((point(:, 2) - line(:, 2)).*dy + (point(:, 1) - line(:, 1)).*dx) ./ (dx.*dx+dy.*dy);
% p0 = line(:, 1:2) + [tp tp].*[dx dy];
% 
% 
% % compute distances between points and their projections
% dx = point - p0;
% dist  = sqrt(sum(dx.*dx, 2));

function alpha = normalizeAngle(alpha, varargin)
%NORMALIZEANGLE  Normalize an angle value within a 2*PI interval.
%
%   ALPHA2 = normalizeAngle(ALPHA);
%   ALPHA2 is the same as ALPHA modulo 2*PI and is positive.
%
%   ALPHA2 = normalizeAngle(ALPHA, CENTER);
%   Specifies the center of the angle interval.
%   If CENTER==0, the interval is [-pi ; +pi]
%   If CENTER==PI, the interval is [0 ; 2*pi] (default).
%
%   Example:
%   % normalization between 0 and 2*pi (default)
%   normalizeAngle(5*pi)
%   ans =
%       3.1416
%
%   % normalization between -pi and +pi
%   normalizeAngle(7*pi/2, 0)
%   ans =
%       -1.5708
%
%   See also
%   vectorAngle, lineAngle
%

% ------
% Author: David Legland
% e-mail: david.legland@nantes.inra.fr
% Created: 2008-03-10,    using Matlab 7.4.0.287 (R2007a)
% Copyright 2008 INRA - BIA PV Nantes - MIAJ Jouy-en-Josas.

% HISTORY
% 2010-03-31 rename as normalizeAngle, and add psb to specify interval
%   center

center = pi;
if ~isempty(varargin)
    center = varargin{1};
end

alpha = mod(alpha-center+pi, 2*pi) + center-pi;

function vr = rotateVector(v, angle)
%ROTATEVECTOR Rotate a vector by a given angle.
%
%   VR = rotateVector(V, THETA)
%   Rotate the vector V by an angle THETA, given in radians.
%
%   Example
%   rotateVector([1 0], pi/2)
%   ans = 
%       0   1
%
%   See also
%   vectors2d, transformVector, createRotation
%
% ------
% Author: David Legland
% e-mail: david.legland@grignon.inra.fr
% Created: 2011-04-14,    using Matlab 7.9.0.529 (R2009b)
% Copyright 2011 INRA - Cepia Software Platform.

% precomputes angles
cot = cos(angle);
sit = sin(angle);

% compute rotated coordinates
vr = [cot * v(:,1) - sit * v(:,2) , sit * v(:,1) + cot * v(:,2)];
	
function alpha = vectorAngle(v1, varargin)
%VECTORANGLE Angle of a vector, or between 2 vectors.
%
%   A = vectorAngle(V);
%   Returns angle between Ox axis and vector direction, in Counter
%   clockwise orientation.
%   The result is normalised between 0 and 2*PI.
%
%   A = vectorAngle(V1, V2);
%   Returns the angle from vector V1 to vector V2, in counter-clockwise
%   order, and in radians.
%
%   A = vectorAngle(..., 'cutAngle', CUTANGLE);
%   A = vectorAngle(..., CUTANGLE); % (deprecated syntax)
%   Specifies convention for angle interval. CUTANGLE is the center of the
%   2*PI interval containing the result. See <a href="matlab:doc
%   ('normalizeAngle')">normalizeAngle</a> for details.
%
%   Example:
%   rad2deg(vectorAngle([2 2]))
%   ans =
%       45
%   rad2deg(vectorAngle([1 sqrt(3)]))
%   ans =
%       60
%   rad2deg(vectorAngle([0 -1]))
%   ans =
%       270
%        
%   See also:
%   vectors2d, angles2d, normalizeAngle
%
% ------
% Author: David Legland
% e-mail: david.legland@grignon.inra.fr
% Created: 2007-10-18
% Copyright 2011 INRA - Cepia Software Platform.

%   HISTORY
%   2010-04-16 add psb to specify center interval
%   2011-04-10 add support for angle between two vectors


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

function [tx, ty] = orientedBoxtoPolygon(obox)
%ORIENTEDBOXTOPOLYGON Convert an oriented box to a polygon (set of vertices).
%
%   POLY = orientedBoxToPolygon(OBOX);
%   Converts the oriented box OBOX given either as [XC YC W H] or as 
%   [XC YC W H THETA] into a 4-by-2 array of double, containing coordinates
%   of box vertices. 
%   XC and YC are center of the box. W and H are the width and the height
%   (dimension in the main directions), and THETA is the orientation, in
%   degrees between 0 and 360.
%
%   Example
%     OBOX = [20 10  40 20 0];
%     RECT = orientedBoxToPolygon(OBOX)
%     RECT =
%         -20 -10 
%          20 -10 
%          20  10 
%         -20  10 
%
%
%   See also:
%   polygons2d, orientedBox, drawOrientedBox, rectToPolygon
%

%   ---------
% Author: David Legland
% e-mail: david.legland@nantes.inra.fr
% INRA - TPV URPOI - BIA IMASTE
% created the 06/04/2005.
%

%   HISTORY
%   2011-10-09 rewrite from rectAsPolygon to orientedBoxToPolygon
%   2016: Simplify by JuanPi Carbajal

% extract box parameters
theta = 0;
x = obox(1);
y = obox(2);
w = obox(3) / 2;  % easier to compute with w and h divided by 2
h = obox(4) / 2;
if length(obox) > 4
    theta = obox(5);
end

v = [cosd(theta); sind(theta)];
M = bsxfun (@times, [-1 1; 1 1; 1 -1; -1 -1], [w h]);
tx  = x + M * v;
ty  = y + M(4:-1:1,[2 1]) * v;

if nargout <= 1
  tx = [tx ty];
end