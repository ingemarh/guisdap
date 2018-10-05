function [ tectot,tectop,tecbot ] = IRI_TEC( context,hstart,hend,istep )
%IRI_TEC compute the total ionospheric content
%
%        subroutine IRI_TEC (hstart,hend,istep,tectot,tectop,tecbot)
%-----------------------------------------------------------------------        
% subroutine to compute the total ionospheric content
% INPUT:      
%   hstart  altitude (in km) where integration should start
%   hend    altitude (in km) where integration should end
%   istep   =0 [fast, but higher uncertainty <5%]
%           =1 [standard, recommended]
%           =2 [stepsize of 1 km; best TEC, longest CPU time]
% OUTPUT:
%   tectot  total ionospheric content in tec-units (10^16 m^-2)
%   tectop  topside content (in %)
%   tecbot  bottomside content (in %)
%
% The different stepsizes for the numerical integration are 
% defined as follows (h1=100km, h2=hmF2-10km, h3=hmF2+10km, 
% h4=hmF2+150km, h5=hmF2+250km):
%       istep   h1-h2   h2-h3   h3-h4   h4-h5   h5-hend
%       0       2.0km   1.0km   2.5km   exponential approximation
%       1       2.0km   1.0km   2.5km   10.0km  30.0km
%       2       1.0km   0.5km   1.0km   1.0km   1.0km   
%
%-----------------------------------------------------------------------        

%        logical         expo
%        dimension       step(5),hr(6)
%        logical     	f1reg
%        common  /block1/hmf2,xnmf2,hmf1,f1reg
%     &         /QTOP/Y05,H05TOP,QF,XNETOP,XM3000,HHALF,TAU
%C NEW-GUL------------------------------
%c     &         /QTOP/Y05,H05TOP,QF,XNETOP,XM3000,hht,TAU

%test   
%        save
  if context.NMF2 <= 0 || context.HMF2 <= 0
    tectop = -1.0;
    tecbot = -1.0;
    tectot = -1.0;
    return;
  end
  numHeights = 6;
  step = zeros(numHeights-1,1);
  hr = zeros(numHeights,1);
  expo = false;
  %numstep = 5;
  xnorm = context.NMF2/1000.;
  % NEW-2003: Half-density: XNETOP at htop in [hmf2,1000 km)
  xxx=context.NMF2/2.;
  ht1=context.HMF2;
  xne1=context.NMF2;
  ht2=ht1;
  xne2=xne1;
  hht=0.0;
  % NEW-2003: Half-density: XNETOP at htop in [hmf2,1000 km)

  hr(1) = 100.;
  hr(2) = context.HMF2-10.;
  hr(3) = context.HMF2+10.;
  hr(4) = context.HMF2+150.;
  hr(5) = context.HMF2+250.;
  hr(6) = hend;
  for i=2:numHeights
    if (hr(i) > hend)
      hr(i)=hend;
    end
  end

  if (istep == IRI2012.FAST_TEC)
    step(1)=2.0;
    step(2)=1.0;
    step(3)=2.5;
    step(4)=5.;
    if (hend > hr(5))
      expo=true;
    end
  elseif (istep == IRI2012.STANDARD_TEC)
    step(1)=2.0;
    step(2)=1.0;
    step(3)=2.5;
    step(4)=10.0;
    step(5)=30.0;
  else %if (istep == IRI2012.BEST_TEC)
    step(1)=1.0;
    step(2)=0.5;
    step(3)=1.0;
    step(4)=1.0;
    step(5)=1.0;
  end

  sumtop = 0.0;
  sumbot = 0.0;
  %
  % find the starting point for the integration
  %

  i=0;
  ia=1;
  h = -Inf;
  while hstart > h
    i=i+1;
    h=hr(i);
    if(hstart > h)
      hr(i)=hstart;
      ia=i;
    end
  end
  %
  % start the numerical integration
  %
  i=ia;
  h=hr(i);
  hu=hr(i+1);
  delx = step(i);
  while true
    h = h + delx;
    hh = h;
    if (h >= hu)
      delx = hu - h + delx;
      hx = hu - delx/2.;
      YNE = context.XE_1(hx);
      if((hx > context.HMF2) && (YNE > context.NMF2))
        YNE=context.NMF2;
      end
      yyy = YNE * delx / xnorm;
      i=i+1;
      if(i < numHeights)
        h = hr(i);
        hu = hr(i+1);
        delx = step(i);
      end
    else
      hx = h - delx/2.;
      YNE = context.XE_1(hx);
      if((hx > context.HMF2) && (YNE > context.NMF2))
        YNE=context.NMF2;
      end
      yyy = YNE * delx / xnorm;
    end
    if (hx <= context.HMF2)
      sumbot = sumbot + yyy;
    else
      sumtop = sumtop + yyy;

% NEW-GUL: remember xne2 at ht2 :
      ht2=hx;
      xne2=YNE;
% NEW-GUL------------------------------

    end

% NEW-GUL: interpolate for htop
    if ~((hx <= context.HMF2) || (hht > 0.0))
      if ((xxx <= xne1) && (xxx > xne2))
        hht=ht1+(ht2-ht1)/(xne2-xne1)*(xxx-xne1);
      else
        ht1=ht2;
        xne1=xne2;
      end
    end
% NEW-GUL------------------------------

    if ~expo || (hh < hr(4))
      if (hh < hend && i < numHeights)
        continue;
      end
      zzz = sumtop + sumbot;
      tectop = sumtop / zzz * 100.;
      tecbot = sumbot / zzz * 100.;
      tectot = zzz * context.NMF2;  
      return;
    end
    break;
  end
  num_step = 3;
  hei_top = hr(4);
  hei_end = hend;
  top_end = hei_end - hei_top;
  del_hei = top_end / num_step;
  xntop = context.XE_1(hei_end)/context.NMF2;

  if(xntop <= 0.9999)

    hei_2 = hei_top;
    hei_3 = hei_2 + del_hei;
    hei_4 = hei_3 + del_hei;
    hei_5 = hei_end;

    hss = top_end / 4.;
%       hss = 360.;
    xkk = exp ( - top_end / hss ) - 1.;
    x_2 = hei_2;
    x_3 =hei_top-hss*log(xkk*(hei_3 - hei_top)/top_end + 1.) ;
    x_4 =hei_top-hss*log(xkk*(hei_4 - hei_top)/top_end + 1.);
    x_5 = hei_end;

    ed_2 = context.XE_1(x_2)/context.NMF2;
    if(ed_2 > 1.)
      ed_2=1.;
    end
    ed_3 = context.XE_1(x_3)/context.NMF2;
    if(ed_3 > 1.)
      ed_3=1.;
    end
    ed_4 = context.XE_1(x_4)/context.NMF2;
    if(ed_4 > 1.)
      ed_4=1.;
    end
    ed_5 = xntop;
    if(ed_3 == ed_2)
     ss_2 = ed_3 * (x_3 - x_2);
    else
     ss_2=( ed_3 - ed_2 ) * ( x_3 - x_2 ) / log ( ed_3 / ed_2 );
    end
    if(ed_4 == ed_3)
     ss_3 = ed_4 * (x_4 - x_3);
    else
     ss_3=( ed_4 - ed_3 ) * ( x_4 - x_3 ) / log ( ed_4 / ed_3 );
    end
    if(ed_5 == ed_4)
     ss_4 = ed_5 * (x_5 - x_4);
    else
     ss_4=( ed_5 - ed_4 ) * ( x_5 - x_4 ) / log ( ed_5 / ed_4 );
    end

    ss_t = ss_2 + ss_3 + ss_4;
  else
    ss_t = top_end;
  end

  sumtop = sumtop + ss_t * 1000.;

  zzz = sumtop + sumbot;
  tectop = sumtop / zzz * 100.;
  tecbot = sumbot / zzz * 100.;
  tectot = zzz * context.NMF2;
end

