function [ PCH, PCHdLat ] = ChebyshevPolynomials( LAT, W, N )
  % CALCULATE CHEBYSHEV POLYNOMIALS FIRST AND SECOND KIND TO DEGREE N
  % INPUT:
  %   LAT - angle
  %   W - trigonometric scale for angle
  %   N - maximum degree for polynomial
  % OUTPUT:
  %   PCH - array of Chebyshev polynomials PCH(n+1,1) = T_n(cos(LAT*w)) and PCH(n+1,2) = U_n(cos(LAT*w))
  %   PCHdLat - array of derivatives of polynomials w.r.t. angle
  if nargin < 3 || N < 1
    N = 1;
  end
  M = 2;
  PCH = zeros(N+1,M);
  
  C = cos(double(LAT)*W);
  % PCH(n+1,1) = T_n(cos(LAT*w)) = cos(n*LAT*w)
  % PCH(n+1,2) = U_n(cos(LAT*w)) = sin((n+1)*LAT*w)/sin(LAT*w)
  PCH(0+1,1) = 1;
  PCH(1+1,1) = C;
  PCH(0+1,2) = 1;
  PCH(1+1,2) = 2*C;
  for n=2:N
    for m=1:M
      PCH(n+1,m) = 2*C*PCH(n-1+1,m) - PCH(n-2+1,m);
    end
  end
  if nargout == 2
    % PCHdLat(n+1,1) = -T'_n(cos(LAT*w))*sin(LAT*w)*w = -sin(n*LAT*w)*n*w
    % PCHdLat(n+1,2) = -U'_n(cos(LAT*w))*sin(LAT*w)*w = (T_(n+1)(cos(LAT*w))*(n+1) - U_n(cos((n+1)*LAT*w)*cos(LAT*w))*w/sin(LAT*w)
    PCHdLat = zeros(N+1,M); % latitude derivatives of legendre polynomials
    S = sin(double(LAT)*W);
    CdLat = -S*W;
    %SdLat = C*W;
    PCHdLat(0+1,1) = 0;
    PCHdLat(0+1,2) = 0;
    PCHdLat(1+1,1) = CdLat;
    PCHdLat(1+1,2) = 2*CdLat;
    for n=2:N
      for m=1:M
        PCHdLat(n+1,m) = 2*CdLat*PCH(n-1+1,m) + 2*C*PCHdLat(n-1+1,m) - PCHdLat(n-2+1,m);
      end
    end
  end
end
