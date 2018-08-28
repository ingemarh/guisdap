function [ PromptVd,DynamoVd,Vd ] = StormVd( FLAG,iP,AE,SLT )
%STORMVD Empirical vertical disturbance drifts model
%      SUBROUTINE StormVd(FLAG,iP,AE,SLT,PromptVd,DynamoVd,Vd)
% *******************************************************************
%  Empirical vertical disturbance drifts model
%  After Fejer and Scherliess, JGR, 102, 24047-24056,1997
%*********************************************************************
%  INPUT:
%    AE: AE(in nT) in 1 hour or 15 minute resolution;
%    SLT: Local time(in hrs) for wanted Vd;
%  OUTPUT:
%    PromptVd: Prompt penetration vertical drifts at given conditions;
%    DynamoVd: Disturbane dynamo vertical drifts at given conditions;
%    Vd: PromptVd+DynamoVd;
%*********************************************************************

%c       IMPLICIT REAL*8(A-H,O-Z)
%       IMPLICIT REAL(A-H,O-Z)
%c       REAL*8 AE(1:366*24*4),Coff1(1:5,1:9),Coff15(1:6,1:9)
%       REAL AE(1:366*24*4),Coff1(1:5,1:9),Coff15(1:6,1:9)
%       INTEGER FLAG 
  persistent Coff1 Coff15;
  if isempty(Coff1)
    Coff1 = transpose([ ...
                 0.0124,-0.0168,-0.0152,-0.0174,-0.0704; ...
                -0.0090,-0.0022,-0.0107, 0.0152,-0.0674; ...
                 0.0275, 0.0051,-0.0132, 0.0020,-0.0110; ...
                -0.0022, 0.0044, 0.0095, 0.0036,-0.0206; ...
                 0.0162, 0.0007, 0.0085,-0.0140, 0.0583; ...
                 0.0181, 0.0185,-0.0109,-0.0031,-0.0427; ...
                -0.0057, 0.0002, 0.0086, 0.0149, 0.2637; ...
                -0.0193, 0.0035, 0.0117, 0.0099, 0.3002; ...
                -0.0492,-0.0201, 0.0338, 0.0099, 0.0746]);

    Coff15 = transpose([ ...
      	        0.0177, 0.0118,-0.0006,-0.0152,-0.0174,-0.0704; ...
      	        0.0051,-0.0074,-0.0096,-0.0107, 0.0152,-0.0674; ...
      	        0.0241, 0.0183, 0.0122,-0.0132, 0.0020,-0.0110; ...
      	        0.0019,-0.0010, 0.0001, 0.0095, 0.0036,-0.0206; ...
      	        0.0170, 0.0183, 0.0042, 0.0085,-0.0140, 0.0583; ...
                0.0086, 0.0189, 0.0200,-0.0109,-0.0031,-0.0427; ...
      	       -0.0070,-0.0053,-0.0090, 0.0086, 0.0149, 0.2637; ...
      	       -0.0326,-0.0101, 0.0076, 0.0117, 0.0099, 0.3002; ...
      	       -0.0470,-0.0455,-0.0274, 0.0338, 0.0099, 0.0746]);
  end

%CCCCCCCCCCCCCCCC**Define to variables**CCCCCCCCCCCCCCCCCCCCC
% To 1 h time resolution:
% dAEt_30=AE(t)-AE(t-1 hour);
% dAEt_90=AE(t-1 hour)-AE(t-2 hour);
%
% To 15 MIN time resolution :
% dAEt_7P5=AE(t)-AE(t-15min);
% dAEt_30=AE(t-15)-AE(t-45min);
% dAEt_75=AE(t-45)-AE(t-105min);
%C
%  Following variables are the same to two resolution: 
% AE1_6=average(AE(1-6hours));
% AE7_12=average(AE(7-12hours));
% AE1_12=average(AE(1-12hours));
% AEd1_6=average(X(AE(1-6hours)-130 nT));
% AEd7_12=average(X(AE(7-12hours)-130 nT));
% AEd1_12=average(X(AE(1-12hours)-130 nT));
% AEd22_28=average(X(AE(22-28hours)-130 nT));
% Here X(a)=a, a>0; =0, a<=0;
% Alfa=0,            AE1_6<200 nT;
%      AE1_6/100-2, 200 nT<AE1_6<200 nT;
%      1,            AE1_6>300 nT;
% Beta=exp(-AE1_12/90),  AE1_12>=70nT;
%      0.46,              AE1_12<70 nT;
%CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCccccccc
%*****************************************************
%        FLAG>0--> 1 h time resolution
%**************************************************** 

  if (FLAG > 0)
  
    dAEt_30=AE(iP)-AE(iP-1);
    dAEt_90=AE(iP-1)-AE(iP-2);
  
    AE1_6=0.0;
    AEd1_6=0.0;

    for i=-1:-1:-6
      AE1_6=AE1_6+AE(iP+i);
      AEd1_6S=AE(iP+i)-130.0;
      if (AEd1_6S <= 0.0)
        AEd1_6S=0.0;
      end
      AEd1_6=AEd1_6+AEd1_6S;
    end

    AE1_6=AE1_6/6.0;
    AEd1_6=AEd1_6/6.0;
  
    AEd7_12=0.0;

    for i=-7:-1:-12
      AEd7_12S=AE(iP+i)-130.0;
      if (AEd7_12S <= 0.0)
        AEd7_12S=0.0;
      end
      AEd7_12=AEd7_12+AEd7_12S;
    end

    AEd7_12=AEd7_12/6.0;
  
    AE1_12=0.0;

    for i=-1:-1:-12
      AE1_12=AE1_12+AE(iP+i);
    end

    AE1_12=AE1_12/12.0;
  
    AEd22_28=0.0;

    for i=-22:-1:-28
      AEd22_28S=AE(iP+i)-130.0;
      if (AED22_28S <= 0.0)
        AEd22_28S=0.0;
      end
      AEd22_28=AEd22_28+AEd22_28S;
    end

    AEd22_28=AEd22_28/7.0;
    AEd22_28P=AEd22_28-200.0;
    if (AEd22_28P <= 0.0)
      AEd22_28P=0.0;
    end
  
    if (AE1_6 > 300.0)
      Alfa=1.0;
    elseif (AE1_6 > 200.0)
      Alfa=AE1_6/100.0-2.0;
    else
      Alfa=0.0;
    end
  
    if (AE1_12 >= 70.0)
      Beta=exp(-AE1_12/90.0);
    else
      Beta=0.46;
    end
    PromptVd=0.0;
    for J=1:9
      PromptVd=PromptVd +(Coff1(1,J)*dAEt_30 +Coff1(2,J)*dAEt_90 ...
                         )*IRI2012.BSPL4_PTIME(J,SLT);
    end
    DynamoVd=0.0;
    for J=1:9
      DynamoVd=DynamoVd+ ...
                 (Coff1(3,J)*AEd1_6+Coff1(4,J)*Alfa*AEd7_12 ...
                 +Coff1(5,J)*Beta*AEd22_28P)*IRI2012.BSPL4_PTIME(J,SLT);
    end
    Vd=PromptVd+DynamoVd;

    % 1 h time resolution end;
    %********************************************************************
    %                  15 min time resolution
    %********************************************************************

  else
    dAEt_7P5=AE(iP)-AE(iP-1);
    dAEt_30=AE(iP-1)-AE(iP-3);
    dAEt_75=AE(iP-3)-AE(iP-7);

    AE1_6=0.0;
    AEd1_6=0.0;

    for i=-4:-1:-24
      AE1_6=AE1_6+AE(iP+i);
      AEd1_6s=AE(iP+i)-130.;
      if (AEd1_6s <= 0.0)
        AEd1_6s=0.0;
      end
      AEd1_6=AEd1_6+AEd1_6s;
    end

    AE1_6=AE1_6/21.0;
    AEd1_6=AEd1_6/21.0;
  
    AEd7_12=0.0;
    for i=-28:-1:-48
      AEd7_12s=AE(iP+i)-130.0;
      if (AEd7_12s <= 0)
        AEd7_12s=0.0;
      end
      AEd7_12=AEd7_12+AEd7_12s;
    end
    AEd7_12=AEd7_12/21.0;
  
    AE1_12=0.0;
    for i=-4:-1:-48
      AE1_12=AE1_12+AE(iP+i);
    end
    AE1_12=AE1_12/45.0;
  
    AEd22_28=0.0;
    for i=-88:-1:-112
      AEd22_28s=AE(iP+i)-130.;
      if (AEd22_28s <= 0)
        AEd22_28s=0.0;
      end
      AEd22_28=AEd22_28+AEd22_28s;
    end
    AEd22_28=AEd22_28/25.0;
    AEd22_28P=AEd22_28-200.0;
    if (AEd22_28P <= 0.0)
      AEd22_28P=0.0;
    end

    %         AE1_6=0.0
    %         AEd1_6=0.0
    %         AEd7_12=0.0 
    %         AEd22_28P=0.0
    %         AE1_12=0.0
    %         dAEt_7P5=400.
    %         dAEt_30=0.
    %         dAEt_75=0.
    %
    if (AE1_6 > 300.0)
      Alfa=1.0;
    elseif (AE1_6 > 200.0)
      Alfa=AE1_6/100.0-2.0;
    else 
      Alfa=0.0;
    end
  
    if (AE1_12 >= 70.0)
      Beta=exp(-AE1_12/90.0);
    else
      Beta=0.46;
    end
  
    PromptVd=0.0;
    for J=1:9
      PromptVd=PromptVd+(Coff15(1,J)*dAEt_7P5+Coff15(2,J)*dAEt_30 ...
                        +Coff15(3,J)*dAEt_75)*IRI2012.BSPL4_PTIME(J,SLT);
    end
    DynamoVd=0.0;
    for J=1:9
      DynamoVd=DynamoVd +(Coff15(4,J)*AEd1_6+ ...
                          Coff15(5,J)*Alfa*AEd7_12+ ...
                          Coff15(6,J)*Beta*AEd22_28P ...
                           )*IRI2012.BSPL4_PTIME(J,SLT);
    end
    Vd=PromptVd+DynamoVd;
  end

end

