function [ PLG, PLGdLat ] = LegendrePolynomials( LAT, W, N, M )
  % CALCULATE ASSOCIATED LEGENDRE POLYNOMIALS TO DEGREE N AND ORDER M
  % INPUT:
  %   LAT - angle
  %   W - trigonometric scale
  %   N - maximum degree of Associated Legendre polynomials
  %   M - maximum order of Associated Legendre polynomials
  % OUTPUT:
  %   PLG - lower triangle matrix of Associated Legendre polynomials PLG(n+1,m+1) = P_n^m(sin(LAT*w))
  %   PLGdLat - lower triangle matrix of derivatives Associated Legendre polynomials w.r.t. LAT
  if nargin < 3 || N < 1
    N = 1;
  end
  if nargin < 4 || M > N || M < 0
    M = N;
  end
  PLG = zeros(N+1,M+1);
  
  S = sin(double(LAT)*W);
  C = cos(double(LAT)*W);
  % PLG(n+1,m+1) = P_n^m(sin(LAT*w))
  PLG(0+1,0+1) = 1.;
  PLG(1+1,0+1) = S;
  PLG(1+1,1+1) = C;
  for n=2:N
    PLG(n+1,0+1) = ((2*(n-1)+1)*S*PLG(n-1+1,0+1) - (n-1)*PLG(n-2+1,0+1))/n;
    for m=1:min(n,M)
      PLG(n+1,m+1) = (n+m-1)*PLG(n-1+1,m-1+1)*C + S*PLG(n-1+1,m+1);
    end
  end
  if nargout == 2
    % PLGdLat(n+1,m+1) = P'_n^m(sin(LAT*w))*cos(LAT*w)*w
    PLGdLat = zeros(N+1,M+1); % latitude derivatives of legendre polynomials
    SdLat = C*W;
    CdLat = -S*W;
    PLGdLat(0+1,0+1) = 0.;
    PLGdLat(1+1,0+1) = SdLat;
    PLGdLat(1+1,1+1) = CdLat;
    for n=2:N
      PLGdLat(n+1,0+1) = ((2*(n-1)+1)*SdLat*PLG(n-1+1,0+1) ...
                        + (2*(n-1)+1)*S*PLGdLat(n-1+1,0+1) ...
                        - (n-1)*PLGdLat(n-1,0+1))/n;
      for m=1:min(n,M)
        PLGdLat(n+1,m+1) =   (n+m-1)*PLGdLat(n-1+1,m-1+1)*C ...
                           + (n+m-1)*PLG(n-1+1,m-1+1)*CdLat ...
                           + PLGdLat(n-1+1,m+1)*S ...
                           + PLG(n-1+1,m+1)*SdLat;
      end
    end
  end
end
