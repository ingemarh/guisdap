function [ COLUMN ] = SCOLUM( J,CHI,Z,TN,XN )
%SCOLUM evaluates the neutral column density for O, O2, and N2
%:::::::::::::::::::::::::::: SCOLUM ::::::::::::::::::::::::::::::::::
%.... this routine evaluates the neutral column density for O, O2, and N2
%.... see Smith & Smith JGR 1972 p 3592
%.... chi=solar zenith angle, RE & GE radius and grav constant for earth
%.... Modified by P. Richards January 2010 to eliminate need for calling
%.... the MSIS model at grazing incidence
%      SUBROUTINE SCOLUM(J,CHI,Z,TN,XN,COLUMN)

%      IMPLICIT NONE
%      INTEGER I,J
%      REAL ZG,CHI,Z,TNJ,ALTG,GE,GR,RE,RP
%      REAL SH,XP,Y,ERFY2,CHAPFN,RG,HG,XG,EM,F,G,A,B,C,D,GL
%      REAL TN,XI,TINF,GTN
%      REAL XN(3),COLUMN(3),SN(3),M(3),DG(9),T(2),GN(3)
  persistent A B C D F G EM M ERPOL GE ALTG EREQU BM;
  if isempty(A)
    A = 1.0606963;
    B = 0.55643831;
    C = 1.0619896;
    D = 1.7245609;
    F = 0.56498823;
    G = 0.06651874;
    %EM = 1.662E-24;
    EM = 1.0/CODATA2006.avogadroConstant;
    M = [16. ,  32. ,  28.];
    %EREQU = 6371.0e5; % cm
    %ERPOL = 6.357E8; % cm
    EREQU=WGS84.equatorialRadius*100.0; % cm
    ERPOL=WGS84.polarRadius*100.0; % cm
    GE = 980.0; % cm/s^2
    %T = [0.0,0.0];
    ALTG = 0.0;
    %ERFY2 = 0.0;
    %DG = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0];
    %BM = 1.38e-16;
    BM = CODATA2006.boltzmannConstant*1.0E7;
  end
  COLUMN = zeros(3,1);
  GN = zeros(3,1);
  SN = zeros(3,1);
  for I=1:3
    %SN(I)=0.0;
    COLUMN(I)=1.E+25;
  end

  TNJ=TN;     %.. Avoids changing Tn at grazing incidence

  if CHI >= IRI2012.pi/2
    %.. is sza>90.0 degrees

    %..Grazing incidence parameters 
    ALTG=(EREQU+Z)*sin(IRI2012.pi-CHI)-EREQU;
    if ALTG >= 85*1.0E5
      ZG=ALTG*1.E-5;

      %.. Bates-Walker temperature
      XI=(ZG-120.0)*(ERPOL*1.0E-5+120.0)/(ERPOL*1.0E-5+ZG);
      TINF=max(TN,500.0);   %.. Crude TINF
      GTN=max(TINF-(TINF-300.0)*exp(-0.025*XI),180.0);

      %.. Neutral densities are extrapolated from altitude to grazing
      %.. altitude. Weighted average Tn and GTn is used 
      GR=GE*(ERPOL/(ERPOL+Z))^2;   %.. gravity
      for I=1:3
        GN(I)=XN(I)*exp((Z-ALTG)/ ...
          ((BM*(TN+GTN*2)/3)/(EM*M(I)*GR)));
      end
      %..   WRITE(88,'(6F8.2,1P,22E10.2)') Z/1.0E5,ZG,CHI*180/3.1416,TN,GTN
      %.. >      ,TNJ,((XN(I),GN(I),SN(I)),I=1,3)
      %.. Make sure that grazing incidence density is not lower than MSIS
      %.. This is for using non MSIS densities like CTIPe
      TNJ=GTN;
      for I=1:3
        SN(I)=GN(I);
        if SN(I) < XN(I)
          SN(I)=XN(I);
        end
      end
    else
      return;
    end
    %.. sn(1)=o , sn(2)=o2 , sn(3)=n2 , tnj=tn,  gr=gravity, rp=distance
    %.. to pt p, sh=scale height, rg=distance to pt g, hg=scale height at g

  end
  GR=GE*(ERPOL/(ERPOL+Z))^2;
  RP=ERPOL+Z;
  %.. Calculate column densities for each species
  for I=1:3
    SH=(BM*TNJ)/(EM*M(I)*GR);
    XP=RP/SH;
    Y=sqrt(0.5*XP)*abs(cos(CHI));
    if Y > 100.0
      fprintf(1,'WARNING, Y IN COLUMN(I) > 100 %4d,%10.2f,%10.2f,%10.2f,%10.2f,%10.2f,%10.2f,%10.2f\n', I,Z/1.0E5,CHI*57.3,TNJ,EM,M(I),GR,RP);
    end
    if Y > 8
      ERFY2=F/(G+Y);
    else%if Y < 8
      ERFY2=(A+B*Y)/(C+D*Y+Y*Y);
    end

    if CHI <= IRI2012.pi*0.5
      CHAPFN=sqrt(0.5*IRI2012.pi*XP)*ERFY2;
      COLUMN(I)=XN(I)*SH*CHAPFN;
    else

      RG=RP*sin(IRI2012.pi-CHI);
      HG=BM*TNJ/ ...
        (EM*M(I)*GE*(EREQU/(EREQU+ALTG))^2);
      XG=RG/HG;
      COLUMN(I)=sqrt(0.5*IRI2012.pi*XG)*HG*(2.0*SN(I)-XN(I)*ERFY2);
    end
  end

end

