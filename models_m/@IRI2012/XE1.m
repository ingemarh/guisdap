function [ XE1,dXE1 ] = XE1( context, H )
%XE1 DETERMINING ELECTRON DENSITY(M-3) IN THE TOPSIDE IONOSPHERE
%*************************************************************   
%*************** ELECTRON DENSITY ****************************   
%*************************************************************   
%
%        FUNCTION XE1(H)    
%----------------------------------------------------------------
% DETERMINING ELECTRON DENSITY(M-3) IN THE TOPSIDE IONOSPHERE   
% (H=HMF2....2000 KM) BY HARMONIZED BENT-MODEL ADMITTING 
% VARIABILITY OF THE GLOBAL PARAMETERS BETA,ETA,DELTA,ZETA WITH        
% GEOM. LATITUDE, SMOOTHED SOLAR FLUX AND CRITICAL FREQUENCY.     
% BETA,ETA,DELTA,ZETA are computed in IRISUB program and 
% communicated via COMMON / This is the IRI-2001 approach
% [REF.:K.RAWER,S.RAMAKRISHNAN,1978] 
% New options include:
% (1) IRI-corrected: TC3,alg10,hcor1 in COMMON / 
%   TC3     correction term divided by (1500-(hcor1-hmF2))
%   alg10   = alog(10.)
%	hcor1	lower height boundary for correction
% (2) NeQuick:  B2TOP  in COMMON /
%	B2TOP   is the topside scale height that depends on foF2 and 
%           hmF2. 
% Switch for choosing the desired option is itopn in COMMON /BLO11
%   itopn   =0 IRI-2001, =1 IRI-2001-corrected, =2 NeQuick
%           =3 Gulyaeva-0.5 is not yet implemented. 
%----------------------------------------------------------------

%        COMMON  /BLOCK1/HMF2,XNMF2,HMF1,F1REG
%     &          /BLO10/BETA,ETA,DELTA,ZETA
%     &          /BLO11/B2TOP,TC3,itopn,alg10,hcor1
%     &          /QTOP/Y05,H05TOP,QF,XNETOP,xm3000,hhalf,tau
%     &          /ARGEXP/ARGMAX

%        logical 	f1reg              

  if context.itopn == IRI2012.TOPSIDE_NeQuick
    [XE1,dXE1]=IRI2012.TOPQ(H,context.NMF2,context.HMF2,context.B2TOP);
  else

    DXDH = (1000.-context.HMF2)/700.;
    x0 = 300. - context.DELTA;
    xmx0 = (H-context.HMF2)/DXDH;
    dxmx0 = 1.0/DXDH;
    x = xmx0 + x0;
    dx = dxmx0;
    [eptr1,deptr1] = IRI2012.EPTR(x,context.BETA,394.5);
    [eptr2,deptr2] = IRI2012.EPTR(x,100.,300.0);
    eptr1 = eptr1 - IRI2012.EPTR(x0,context.BETA,394.5);
    eptr2 = eptr2 - IRI2012.EPTR(x0,100.,300.0);
    Y = context.BETA * context.ETA * eptr1 + context.ZETA * (100. * eptr2 - xmx0);
    dY = context.BETA * context.ETA * deptr1*dx + context.ZETA * (100. * deptr2*dx - dxmx0);
    Y = Y * DXDH;
    dY = dY * DXDH;
    if abs(Y) > IRI2012.ARGMAX
      Y = IRI2012.ARGMAX*sign(Y);
    end

    if context.itopn == IRI2012.TOPSIDE_GulH05
      if (context.QF == 1.) && (abs(H-context.H05TOP) < 1.)
        context.QF=IRI2012.Y05/Y;
        context.dQF=-IRI2012.Y05*dY/Y/Y;
      end
      XE1 = context.NMF2 * exp(-Y*context.QF);
      dXE1 = (-dY*context.QF-Y*context.dQF)*XE1;
    else
      TCOR = 0.;
      dTCOR = 0.;
      if context.itopn == IRI2012.TOPSIDE_CORRECTED && H >  context.hcor1
        xred = H - context.hcor1;
        dxred = 1.0;
        rco = context.TC3 * xred;
        drco = context.TC3 * dxred;
        TCOR = rco * IRI2012.alg10;
        dTCOR = drco * IRI2012.alg10;
      end
      XE1 = context.NMF2 * exp(-Y+TCOR);
      dXE1 = (-dY+dTCOR)*XE1;
    end
  end
end

