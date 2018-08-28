function [ NMAX, ERAD, GH, IER ] = GETSHC( context, FSPEC )
%GETSHC Reads spherical harmonic coefficients from the specified file into an array
%        SUBROUTINE GETSHC (IU, FSPEC, NMAX, ERAD, GH, IER)                                                                                           
% ===============================================================               
%       Reads spherical harmonic coefficients from the specified     
%       file into an array.                                          
%       Input:                                                       
%           IU    - Logical unit number                              
%           FSPEC - File specification                               
%       Output:                                                      
%           NMAX  - Maximum degree and order of model                
%           ERAD  - Earth's radius associated with the spherical     
%                   harmonic coefficients, in the same units as      
%                   elevation                                        
%           GH    - Schmidt quasi-normal internal spherical          
%                   harmonic coefficients                            
%           IER   - Error number: =  0, no error                     
%                                 = -2, records out of order         
%                                 = FORTRAN run-time error number    
% ===============================================================               

%        CHARACTER  FSPEC*(*), FOUT*80                                    
%        DIMENSION       GH(196) 
%        COMMON/iounit/konsol        
  NMAX = 0;
  ERAD = 0.0;
  GH = zeros(IGRF.numCoeff,1);

  % ---------------------------------------------------------------               
  %       Open coefficient file. Read past first header record.        
  %       Read degree and order of model and Earth's radius.           
  % ---------------------------------------------------------------               
  FOUT = FSPEC;
  % 667    FORMAT(A13)
  % 667    FORMAT('/var/www/omniweb/cgi/vitmo/IRI/',A13)
  IU = fopen (FOUT, 'r');
  if IU == -1
    IER = -1;
    if context.KONSOL > 0
      fprintf(context.KONSOL,'Error while reading %s\n', FOUT);
    end
    return;
  end
  sc = textscan(IU,'%s');
  sc = sc{1};
  fclose (IU);
  if isempty(sc)
    IER = -2;
    if context.KONSOL > 0
      fprintf(context.KONSOL,'Error while parsing %s\n', FOUT);
    end
    return;
  end
  NMAX = str2double(sc(2));
  ERAD = str2double(sc(3));
  %MYEAR = str2double(sc(4));
  nm=NMAX*(NMAX+2);
  for i=1:nm
    GH(i) = str2double(sc(4+i));
  end

  IER = 0;

end

