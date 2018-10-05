function [ NMAX, GH ] = EXTRASHC( DATE, DTE1, NMAX1, GH1, NMAX2, GH2 )
%EXTRASHC Extrapolates linearly a spherical harmonic model with a        
% rate-of-change model
%        SUBROUTINE EXTRASHC (DATE, DTE1, NMAX1, GH1, NMAX2,           
%                              GH2, NMAX, GH)                           
%                                                                                
% ===============================================================               
%                                                                               
%       Version 1.01                                                   
%                                                                               
%       Extrapolates linearly a spherical harmonic model with a        
%       rate-of-change model.                                          
%                                                                               
%       Input:                                                         
%           DATE  - Date of resulting model (in decimal year)          
%           DTE1  - Date of base model                                 
%           NMAX1 - Maximum degree and order of base model             
%           GH1   - Schmidt quasi-normal internal spherical            
%                   harmonic coefficients of base model                
%           NMAX2 - Maximum degree and order of rate-of-change         
%                   model                                              
%           GH2   - Schmidt quasi-normal internal spherical            
%                   harmonic coefficients of rate-of-change model      
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
%       DATE, are computed by linearly extrapolating the coef-         
%       ficients of the base model (GH1), at date DTE1, using          
%       those of the rate-of-change model (GH2), at date DTE2. If      
%       one model is smaller than the other, the extrapolation is      
%       performed with the missing coefficients assumed to be 0.       
% --------------------------------------------------------------- 

%        DIMENSION       GH1(*), GH2(*), GH(*)                        
%                                                                                
                                                                               
  FACTOR = double(DATE - DTE1);

  if NMAX1 == NMAX2
    K = NMAX1 * (NMAX1 + 2);
    NMAX = NMAX1;
    GH = zeros(K,1);
  elseif NMAX1 > NMAX2
    K = NMAX2 * (NMAX2 + 2);
    L = NMAX1 * (NMAX1 + 2);
    GH = zeros(L,1);
    for I = K + 1: L
      GH(I) = GH1(I);
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
    GH(I) = GH1(I) + FACTOR * GH2(I);
  end

end

