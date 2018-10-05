function [ context,LMAX ] = PARAMS( context,ISW )
%PARAMS cross sections, solar fluxes
%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%      SUBROUTINE PARAMS(ISW,LMAX)
%........ this program determines the cross sections, solar fluxes, and
%........ given in m. torr et al g.r.l 1979 p771, table 2 and 3. but
%........ the longer wavelengths come first

%      IMPLICIT NONE
%      INTEGER  I,IN,IS,ISW,J,L,LAMAX,LMAX,NNI
%      REAL EUV,FFAC, SIGABS,SIGION,TPOT,UVFAC,ZFLUX,ZLAM
%      REAL  X1(111),X2(111),X3(18),ZLX(37),ZFX(37)
%      COMMON/SIGS/ZFLUX(37),SIGABS(3,37),ZLAM(37),SIGION(3,37),
%     > TPOT(3,10),NNI(3),LAMAX
%      COMMON/SOL/UVFAC(59),EUV
%

  persistent X3 ZLX ZFX X1 X2;
  if isempty(X3)
    %....... ionization potentials for o,o2 and n2 see kirby et al note the
    %....... o2 2(pi)u and 2(sigma-)u , and n2 f2(sigma)u pots are guesses
    %....... the sixth n2 potential is for dissociation
    X3 = [13.6,16.9,18.6,28.5,40.,0.0,12.1,16.5,18.2,20., ...
       25.,0.,15.6,16.7,18.8,25.,29.,37.];
    %........ wavelength data. average is taken for bands
    ZLX = [1025.,1031.91,1025.72,975.,977.02,925.,875.,825.,775., ...
       789.36,770.41,765.15,725.,703.36,675.,625.,629.73,609.76,575., ...
       584.33,554.31,525.,475.,465.22,425.,375.,368.07,325.,303.78, ...
       303.31,275.,284.15,256.3,225.,175.,125.,75.];
    %........ fluxes from table 3. these are for 74113. just replace this data
    %........ for other years in the table. note!!!! flux doubled for lambda<250
    %........ shortest wavelenghts have been tripled
    ZFX = [2.4665,2.1,3.5,1.4746,4.4,3.,3.537,1.625,.758,.702, ...
       .26,.17,.141,.36,.23,.342,1.59,.53,.357,1.27,.72,.452,.285, ...
       .29,.383,.314,.65,.965,6.9,.8,1.679,.21,.46,3.1,4.8,.45,1.2];
    %........ absorption cross sections -- o first ,o2, then n2
    X1 = [0.0,0.0,0.0,0.0,0.0,1.315,4.554,3.498,5.091,3.749,3.89,4,10.736,11.46 ...
       ,17.245,13.365,13.4,13.4,13.024,13.09,12.59,12.059,12.127,11.93 ...
       ,11.496,9.687,9.84,8.693,7.7,7.68,6.461,7.08,6.05,5.202,3.732 ...
       ,1.839,.73,  1.346,1.0,1.63,21.108,18.73,12.817,8.562,16.631 ...
       ,22.145,26.668,18.91,20.8,28.535,27.44,21.919,26.017,32.06 ...
       ,28.07,26.61,22.79,26.04,24.606,23.101,21.91,20.31,18.118 ...
       ,18.32,17.438,16.81,16.8,14.387,15.79,13.37,10.9,7.509,3.806 ...
       ,1.316,0.0,0.0,0.0,50.988,2.24,9.68,20.249,16.992,33.578,16.487 ...
       ,14.18,120.49,24.662,26.54,31.755,23.339,23.37,22.79,22.787 ...
       ,22.4,24.13,24.501,23.471,23.16,21.675,16.395,16.91,13.857 ...
       ,11.7,11.67,10.493,10.9,10.21,8.392,4.958,2.261,0.72];
    %....... ionization cross sections 
    X2 = [0.0,0.0,0.0,0.0,0.0,1.315,4.554,3.498,5.091,3.749,3.89,4,10.736,11.46 ...
       ,17.245,13.365,13.4,13.4,13.024,13.09,12.59,12.059,12.127,11.93 ...
       ,11.496,9.687,9.84,8.693,7.7,7.68,6.461,7.08,6.05,5.202,3.732 ...
       ,1.839,.73,  .259,0.0,1.05,13.94,15.54,9.374,5.494,6.413,10.597 ...
       ,10.191,8.47,11.72,23.805,23.75,21.306,24.937,31.1,26.39 ...
       ,26.61,22.79,26.04,24.606,23.101,21.91,20.31,18.118 ...
       ,18.32,17.438,16.81,16.8,14.387,15.79,13.37,10.9,7.509,3.806 ...
       ,1.316,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,14.274,8.86,8.5,65.8,15.06,25.48,29.235 ...
       ,23.339,23.37,22.79,22.787 ...
       ,22.4,24.13,24.501,23.471,23.16,21.675,16.395,16.91,13.857 ...
       ,11.7,11.67,10.493,10.9,10.21,8.392,4.958,2.261,0.72];
  end
  if context.KONSOL <= 0
    ISW = 0;
  end
  context.NNI(1)=5;
  context.NNI(2)=5;
  context.NNI(3)=6;
  LMAX=37;
  if ISW ~= 0
    fprintf(context.KONSOL,...
      '    ,EUV fluxes, Photoabsorption, and Photoionization Cross sections    ,I,    ,lam,     ,flux,    ,sigaOX,   ,sigaO2,   ,sigaN2,   ,sigiOX,   ,sigiO2,   ,sigiN2,   ,UVfac');
  end

  for L=1:LMAX
    context.ZLAM(L)=ZLX(L);
    FFAC=context.UVFAC(LMAX+1-L);
    if ZFX(L) < 100
      context.ZFLUX(L)=ZFX(L)*1.0E+9*FFAC;
    end
    %..- setting up ionization potentials
    if L <= 6
       context.TPOT(1,L)=X3(L);
       context.TPOT(2,L)=X3(6+L);
       context.TPOT(3,L)=X3(12+L);
    end
    %..- setting up cross sections
    for IS=1:3
      IN=LMAX*(IS-1)+L;
      context.SIGABS(IS,L)=X1(IN)*1.0E-18;
      context.SIGION(IS,L)=X2(IN)*1.0E-18;
      if context.SIGABS(IS,L) < context.SIGION(IS,L)
        context.SIGABS(IS,L)=context.SIGION(IS,L);
      end
    end

    if ISW~= 0
      for I=1:3
        fprintf(context.KONSOL,...
          ' ,%4d,%9.2f,%d,%9.2f',...
          L,context.ZLAM(L),context.ZFLUX(L),context.SIGABS(I,L),context.SIGION(I,L),FFAC);
      end
    end
  end

  if ISW == 0
    return;
  end
  fprintf(context.KONSOL,...
    '    , Ionization potentials for O, O2, N2  ,4S   2D   2P   4P   2P*  -   X2   a+A  b4   B2   dis  -  X2   A2   B2   C2   F2   2s');

  for J=1:6
    for I=1:3
      fprintf(1,' %5.1f', context.TPOT(I,J));
    end
  end

end

