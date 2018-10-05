function [ XE4_1,dXE4_1 ] = XE4_1( context, H )
%XE4_1 ELECTRON DENSITY FOR THE INTERMEDIATE REGION
%        REAL FUNCTION XE4_1(H)
% ELECTRON DENSITY FOR THE INTERMEDIATE REGION (HEF...HZ)
% USING THE NEW DEFINED FUNCTION

%        COMMON	/BLOCK3/	HZ,T,HST
%     &		/BLOCK4/	HME,XNME,HEF
%
  if context.HST < 0.0
		XE4_1=context.NME+context.T*(H-context.HEF);
		dXE4_1=context.T;
  else
    if context.HST == context.HEF
       H1BAR=H;
       dH1BAR=1.0;
    else
      if context.T >= 0
       H1BAR=context.HZ+0.5*context.T ...
         -sqrt(context.T*(0.25*context.T+context.HZ-H));
       dH1BAR=0.5*context.T/sqrt(context.T*(0.25*context.T+context.HZ-H));
      else
       H1BAR=context.HZ+0.5*context.T ...
         +sqrt(context.T*(0.25*context.T+context.HZ-H));
       dH1BAR=-0.5*context.T/sqrt(context.T*(0.25*context.T+context.HZ-H));
      end
    end
    [XE4_1,~,dXE4_1]=context.XE3_1(H1BAR);
    dXE4_1 = dXE4_1*dH1BAR;
  end

end

