function [ NMAX, GH ] = INTERSHC( DATE, DTE1, NMAX1, GH1, DTE2, NMAX2, GH2 )
%INTERSHC Interpolates linearly, in time, between two spherical harmonic models
%        SUBROUTINE INTERSHC (DATE, DTE1, NMAX1, GH1, DTE2,          
%                              NMAX2, GH2, NMAX, GH)                  
%                                                                                
% ===============================================================               
%                                                                               
%       Version 1.01                                                 
%                                                                               
%       Interpolates linearly, in time, between two spherical        
%       harmonic models.                                             
%                                                                               
%       Input:                                                       
%           DATE  - Date of resulting model (in decimal year)        
%           DTE1  - Date of earlier model                            
%           NMAX1 - Maximum degree and order of earlier model        
%           GH1   - Schmidt quasi-normal internal spherical          
%                   harmonic coefficients of earlier model           
%           DTE2  - Date of later model                              
%           NMAX2 - Maximum degree and order of later model          
%           GH2   - Schmidt quasi-normal internal spherical          
%                   harmonic coefficients of later model             
%                                                                               
%       Output:                                                      
%           GH    - Coefficients of resulting model                  
%           NMAX  - Maximum degree and order of resulting model      
%                                                                               
%       A. Zunde                                                     
%       USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225    
%                                                                               
% ===============================================================               
%                                                                                
% ---------------------------------------------------------------               
%       The coefficients (GH) of the resulting model, at date        
%       DATE, are computed by linearly interpolating between the     
%       coefficients of the earlier model (GH1), at date DTE1,       
%       and those of the later model (GH2), at date DTE2. If one     
%       model is smaller than the other, the interpolation is        
%       performed with the missing coefficients assumed to be 0.     
% ---------------------------------------------------------------               

%        DIMENSION       GH1(*), GH2(*), GH(*)                        

  FACTOR = double(DATE - DTE1) / double(DTE2 - DTE1);

  if NMAX1 == NMAX2
    K = NMAX1 * (NMAX1 + 2);
    NMAX = NMAX1;
    GH = zeros(K,1);
  elseif NMAX1 > NMAX2
    K = NMAX2 * (NMAX2 + 2);
    L = NMAX1 * (NMAX1 + 2);
    GH = zeros(L,1);
    for I = K + 1: L
      GH(I) = GH1(I) + FACTOR * (-GH1(I));
    end
    NMAX = NMAX1;
  else
    K = NMAX1 * (NMAX1 + 2);
    L = NMAX2 * (NMAX2 + 2);
    GH = zeros(K,1);
    for I = K + 1: L
      GH(I) = FACTOR * GH2(I);
    end
    NMAX = NMAX2;
  end

  for I = 1: K                                                  
    GH(I) = GH1(I) + FACTOR * (GH2(I) - GH1(I));
  end
                                                                                
end

