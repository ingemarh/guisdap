function [ SIGOX,SIGN2,SIGEE ] = SIGEXS( E,TE,XNE )
%SIGEXS evaluating the total inelastic cross sections
%::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%     SUBROUTINE SIGEXS(E,TE,XNE,SIGOX,SIGN2,SIGEE)
%..... Program for evaluating the total inelastic cross sections
%
%........ loss to thermal electrons ....
  ET=8.618E-5*TE;
  if E ~= 0
    SIGEE=(3.37E-12/E^0.94/XNE^0.03)*((E-ET)/ ...
       (E-(0.53*ET)))^2.36;
  else
    SIGEE=0;
  end
%
%...... cross section for o(1d)
  SIGO1D=0.0;
  if(E > 1.96)
    SIGO1D=4E-16*(1-1.96/E)^2/E;
  end
%...... total excitation cross section for O excluding O(1D)
  if(E < 25)
    SIGO=(0.4*E-5)*1.4E-17;
  end
  if(E >= 25)
    SIGO=7.0E-17;
  end
  if(SIGO < 0.0)
    SIGO=0.0;
  end
%
%...... total excitation cross section for N2......
  if(E < 12)
    SIGN2=(15.5*E-104.8)*1.7E-18;
  end
  if(E < 4.0)
    SIGN2=5.0E-9*(1-1.4/E)^9 * (1.4/E)^16;
  end
  if(E > 11.5)
    SIGN2=1.4E-16;
  end
  if(SIGN2 < 0.0)
    SIGN2=0.0;
  end
%
%........ total ionization cross sections from Keiffer and Dunn ....
  SIGION=0.0;
  AL=log10(E);
  if(AL < 2.7 && AL >= 1.2)
    SIGION=-3.6E-16*(AL-1.2)*(AL-3);
  end
  if(AL > 2.7)
    SIGION=1.2E-14*EXP(-AL*1.6);
  end
  if(E < 50)
    SIGION=1.0E-16*(0.068*E-1.06);
  end
  if(SIGION <= 0.0)
    SIGION=0.0;
  end
%
  SIGOX=SIGO1D+SIGO+0.5*SIGION;
  SIGN2=SIGN2+SIGION;

end

