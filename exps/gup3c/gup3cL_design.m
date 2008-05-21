function gup3c_L120_design  %{

%FUNCTION GUP3C_L120_DESIGN
%GUISDAP design function for experiment gup3c, version 1.20
%
%       IPP1           IPP2               IPP1            IPP2
%  LP(f1) AC1(f2) | LP(f3) AC1(f4) || LP(f1) AC2(f2) | LP(f3) AC2(f4) || ...
%               CYCLE 1/32         ||             CYCLE 2/32          ||
%       s1        |      s3        ||      
%       s2        |      s4        ||      
%       b3c3      |      b1c1      ||      
%       b4c4      |      b2c2      ||      
%
% This version has 2 x long pulse and 2 x 16-bit alt code, with 1/2-integer
% lags.

% 24-Jul-1998 Jm
%===========================================================================

    Nsamples    = 244;      % ???
    Ninj        = 4;

    LPrfon      = 50;
    ACrfon      = 710;

    Chon        = 1470;
    Tau         = 20;

    Ipp         = 6510;
    Cycle       = 2*Ipp;
    Ncycles     = 32;

    Tunit       = 2
%   Taps        = ones(1,10);
    [Taps,T0]   = get_impresp('/esrsw/esr0/dsp/fir/w25d200.fir',Tunit,0);

    LP1          = CHANNEL(1);
    LP1.tau      = Tau;
    LP1.baud     = 640;
    LP1.fstlag   = 0;
    LP1.maxlag   = 20*Tau;
    LP1.nbits    = 1;

    AC1          = CHANNEL(2);
    AC1.tau      = Tau;
    AC1.baud     = 2*Tau;
    AC1.fstlag   = Tau;
    AC1.maxlag   = 15.5*2*Tau;
    AC1.nbits    = 16;

    LP2          = CHANNEL(3,LP1);
    AC2          = CHANNEL(4,AC1);

%------------------------------------------------
    t_choff  = Chon+(Nsamples-1)*Tau
    t_calon  = t_choff+2*Tau
    t_caloff = t_calon + (Ninj-1)*Tau

    Bwindow  = [Chon t_choff]
    Cwindow  = [t_calon t_caloff]
    
%------------------------------------------------

    DS_start('ESR',32000,1);
    DS_set('clear');

% Calibration temperature

    global p_calTemp;  p_calTemp = 220;

% Basic time unit for the design

    global p_dtau;  p_dtau = Tunit;

% Sampling

    DS_set('adcint',Tau);

% PDF filtering  

    DS_set('taps',Taps);

% Cycling

    DS_set('cycle',Cycle);
    DS_set('Ncycles',Ncycles)

%------------------------------------------------------
%   LP1 (f1) :  [LP1] ac1 | lp2 ac2
%------------------------------------------------------

% Transmission and signal reception is during IPP_A

    TRANSMIT(LP1,LPrfon);
    RECEIVE(LP1,Nsamples,Chon)

%   Background and noise injection sampled during Ipp_B

    CALIBRATE(LP1, Ipp + Bwindow, Ipp + Cwindow)

    DS_set('go')

%------------------------------------------------------
%   LP2 (f3) :  lp1 ac1 | [LP2] ac2
%------------------------------------------------------

% Transmission and signal reception is during Ipp_B

    TRANSMIT(LP2, Ipp + LPrfon);
    RECEIVE(LP2, Nsamples, Ipp + Chon)

% Background and noise injection are sampled during Ipp_A

    CALIBRATE(LP2, Bwindow, Cwindow)

    DS_set('go')


%------------------------------------------------------
%   AC1 (f2) :  lp1 [AC1] | lp2 ac2
%------------------------------------------------------

% Transmission and signal reception is during Ipp_A

    TRANSMIT(AC1, ACrfon);
    RECEIVE(AC1, Nsamples, Chon)

% Background and noise injection are taken during Ipp_B

    CALIBRATE(AC1, Ipp + Bwindow, Ipp + Cwindow)

    DS_set('go')


%------------------------------------------------------
%   AC2 (f4) :  lp1 ac1 | lp2 [AC2]
%------------------------------------------------------

% Transmission and signal reception is during Ipp_B

    TRANSMIT(AC2, Ipp + ACrfon);
    RECEIVE(AC2, Nsamples, Ipp + Chon)

% Background and noise injection are taken during Ipp_A

    CALIBRATE(AC2, Bwindow, Cwindow)

    DS_set('go')


%}main


function TRANSMIT(X,rfon) %{
%------------------------
    if nargin ~= 2
        error('Wrong number of input arguments')
    end    
    DS_set('channel',X.ch)
    DS_set('XMITtime',rfon)
    DS_set('bitlen',X.baud)
    if X.nbits == 1
        DS_set('Npulses',1)
    else
        DS_set('Nbits',X.nbits)    
    end
%}TRANSMIT    


function CH = CHANNEL(Ch,Template) %{
%---------------------------------
    if nargin == 2
        CH = Template;
        CH.ch = Ch;
    elseif nargin == 1
        CH.ch        = Ch;
        CH.nbits     = [];
        CH.baud      = [];
        CH.tau       = [];
        CH.fstlag    = [];
        CH.maxlag    = [];
    else
        error('Wrong number of input arguments')    
    end
%}CHANNEL    


function RECEIVE(CH,N,ChOn) %{   
%-------------------------
    if nargin ~= 3
        error('Wrong number of input arguments')
    end
    if isempty(CH.ch)
        error('CH must be specified');
    end    
    if isempty(CH.nbits)
        error(EMSG(CH,'NBITS not specified'));
    end
    if isempty(CH.tau)
        error(EMSG(CH,'TAU not specified'));
    end
    if isempty(CH.fstlag)
        if CH.nbits == 1
            CH.fstlag = 0;
        else
            CH.fstlag = CH.tau;
        end        
    end
    if isempty(CH.maxlag)
        error(EMSG(CH,'MAXLAG not specified'))
    end    
    if rem(CH.fstlag,CH.tau) ~= 0
        error(EMSG(CH,'FSTLAG not an integer multiple of TAU'))
    end
    if rem(CH.maxlag,CH.tau) ~= 0
        error(EMSG(CH,'MAXLAG not an integer multiple of TAU'))
    end
    if rem(CH.baud,CH.tau) ~= 0
        error(EMSG(CH,'BAUD not an integer multiple of TAU'));
    end    

    DS_set('RECtime', ChOn);
    if CH.nbits == 1
        DS_set('gates',N);
        DS_set('lags', CH.fstlag:CH.tau:CH.maxlag);
    else
        DS_set('gates',N - (CH.nbits-1)*round(CH.baud/CH.tau)); 
        DS_set('lags', CH.fstlag:CH.tau:CH.maxlag);
    end        

%}RECEIVE


function emsg = EMSG(CH,msg) %{
%---------------------------
    if nargin ~= 2
        error('Wrong number of input arguments');
    end    
    emsg = sprintf('Ch %d: %s',CH.ch, msg);
%}EMSG    


function CALIBRATE(CH,Bwindow,Cwindow)
%-------------------------------------    
    DS_set('Bwindow',Bwindow);
    if CH.nbits == 1
        DS_set('Blags',0:CH.tau:CH.maxlag)
    else    
        DS_set('Blags',0)
    end
    DS_set('Cwindow',Cwindow);
%}CALIBRATE


