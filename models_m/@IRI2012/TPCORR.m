function [ TP350A,TP350B,TP550A,TP550B,TP850A,TP850B,TP140A,TP140B,TP200A,TP200B ] ...
  = TPCORR( INVDIP,MLT,DDD,PF107,P350A,P350B,P550A,P550B,P850A,P850B,P1400A,P1400B, ...
  P2000A,P2000B )
%TPCORR TPCORR
%
%      SUBROUTINE   TPCORR(INVDIP,MLT,DDD,PF107,
%                   P350A,P350B,P550A,P550B,P850A,P850B,
%                   P1400A,P1400B,P2000A,P2000B, 
%                   TP350A,TP350B,TP550A,TP550B,TP850A,TP850B,
%                   TP140A,TP140B,TP200A,TP200B)
%-------------------------------------------------------------------------------

%       REAL INVDIP,MLT,PF107
%       INTEGER DDD
%       REAL        P350A,P350B,P550A,P550B,P850A,P850B,
%     &             P1400A,P1400B,P2000A,P2000B, 
%     &             TP350A,TP350B,TP550A,TP550B,TP850A,TP850B,
%     &             TP140A,TP140B,TP200A,TP200B
%       REAL INTERP
%       REAL MLTRAD
%      Constants    
%       REAL INVDPQ(13)
%      PF107 Day Equinox       
%       REAL P2DE(13,3),P1DE(13,3),P8DE(13,3),P5DE(13,3),P3DE(13,3)
%      Te max-min dif Day Equinox       
%       REAL CXN2DE(13),CXN1DE(13),CXN8DE(13),CXN5DE(13),CXN3DE(13)
%      Te med-min dif Day Equinox       
%       REAL CDN2DE(13),CDN1DE(13),CDN8DE(13),CDN5DE(13),CDN3DE(13)
%
%      PF107 Night Equinox       
%       REAL P2NE(13,3),P1NE(13,3),P8NE(13,3),P5NE(13,3),P3NE(13,3)
%      Te max-min dif Night Equinox       
%       REAL CXN2NE(13),CXN1NE(13),CXN8NE(13),CXN5NE(13),CXN3NE(13)
%      Te med-min dif Night Equinox       
%       REAL CDN2NE(13),CDN1NE(13),CDN8NE(13),CDN5NE(13),CDN3NE(13)
%       
%
%      PF107 Day Solstice       
%       REAL P2DS(13,3),P1DS(13,3),P8DS(13,3),P5DS(13,3),P3DS(13,3)
%      Te max-min dif Day Solstice       
%       REAL CXN2DS(13),CXN1DS(13),CXN8DS(13),CXN5DS(13),CXN3DS(13)
%      Te med-min dif Day Solstice       
%       REAL CDN2DS(13),CDN1DS(13),CDN8DS(13),CDN5DS(13),CDN3DS(13)
%
%      PF107 Night Solstice       
%       REAL P2NS(13,3),P1NS(13,3),P8NS(13,3),P5NS(13,3),P3NS(13,3)
%      Te max-min dif Night Solstice       
%       REAL CXN2NS(13),CXN1NS(13),CXN8NS(13),CXN5NS(13),CXN3NS(13)
%      Te med-min dif Night Solstice       
%       REAL CDN2NS(13),CDN1NS(13),CDN8NS(13),CDN5NS(13),CDN3NS(13)
%      working variables
%       REAL TXN2DE(13),TXN1DE(13),TXN8DE(13),TXN5DE(13),TXN3DE(13)
%       REAL TDN2DE(13),TDN1DE(13),TDN8DE(13),TDN5DE(13),TDN3DE(13)
%       REAL TXN2NE(13),TXN1NE(13),TXN8NE(13),TXN5NE(13),TXN3NE(13)
%       REAL TDN2NE(13),TDN1NE(13),TDN8NE(13),TDN5NE(13),TDN3NE(13)
%       REAL TXN2DS(13),TXN1DS(13),TXN8DS(13),TXN5DS(13),TXN3DS(13)
%       REAL TDN2DS(13),TDN1DS(13),TDN8DS(13),TDN5DS(13),TDN3DS(13)
%       REAL TXN2NS(13),TXN1NS(13),TXN8NS(13),TXN5NS(13),TXN3NS(13)
%       REAL TDN2NS(13),TDN1NS(13),TDN8NS(13),TDN5NS(13),TDN3NS(13)      
%
%      Interpolated PF107
%       REAL P2DEI(3),P1DEI(3),P8DEI(3),P5DEI(3),P3DEI(3)
%       REAL P2NEI(3),P1NEI(3),P8NEI(3),P5NEI(3),P3NEI(3)
%       REAL P2DSI(3),P1DSI(3),P8DSI(3),P5DSI(3),P3DSI(3)
%       REAL P2NSI(3),P1NSI(3),P8NSI(3),P5NSI(3),P3NSI(3) 
%      Additional local and temporary variables
%       REAL XN2DEI,XN1DEI,XN8DEI,XN5DEI,XN3DEI
%       REAL XN2NEI,XN1NEI,XN8NEI,XN5NEI,XN3NEI
%       REAL DN2DEI,DN1DEI,DN8DEI,DN5DEI,DN3DEI
%       REAL DN2NEI,DN1NEI,DN8NEI,DN5NEI,DN3NEI
%       REAL XN2DSI,XN1DSI,XN8DSI,XN5DSI,XN3DSI
%       REAL XN2NSI,XN1NSI,XN8NSI,XN5NSI,XN3NSI
%       REAL DN2DSI,DN1DSI,DN8DSI,DN5DSI,DN3DSI
%       REAL DN2NSI,DN1NSI,DN8NSI,DN5NSI,DN3NSI
%       REAL MLTTMP
%       INTEGER I      
%
  persistent INVDPQ P2DE CXN2DE CDN2DE P1DE CXN1DE CDN1DE P8DE CXN8DE CDN8DE ...
     P5DE CXN5DE CDN5DE P3DE CXN3DE CDN3DE P2NE CXN2NE CDN2NE P1NE CXN1NE ...
     CDN1NE P8NE CXN8NE CDN8NE P5NE CXN5NE CDN5NE P3NE CXN3NE CDN3NE P2DS ...
     CXN2DS CDN2DS P1DS CXN1DS CDN1DS P8DS CXN8DS CDN8DS P5DS CXN5DS CDN5DS ...
     P3DS CXN3DS CDN3DS P2NS CXN2NS CDN2NS P1NS CXN1NS CDN1NS P8NS CXN8NS ...
     CDN8NS P5NS CXN5NS CDN5NS P3NS CXN3NS CDN3NS NI NJ;
  if isempty(INVDPQ)
    NI = 13;
    NJ = 3;
    INVDPQ = [ ...
      -90.0,-75.0,-60.0,-45.0,-30.0,-15.0, 0.0, 15.0, 30.0, 45.0, 60.0, 75.0, 90.0];
    %      Equinox
    P2DE = transpose([ ...
      125., 120., 115., 113., 118., 119., 118., 119., 118., 113., 115., 120., 125.; ...
      150., 151., 152., 158., 154., 156., 147., 156., 154., 158., 152., 151., 150.; ...
      181., 197., 213., 214., 205., 199., 197., 199., 205., 214., 213., 197., 181.]);
    CXN2DE = transpose([ ...
      587., 466., 345., 264., 355., 335., 229., 335., 355., 264., 345., 466., 587.]);
    CDN2DE = transpose([ ...
      244.,  93., -59.,-103.,  57., 170.,  16., 170.,  57.,-103., -59.,  93., 244.]);
    P1DE = transpose([ ...
      111., 110., 109., 109., 109., 110., 109., 110., 109., 109., 109., 110., 111.; ...
      135., 135., 134., 128., 131., 145., 148., 145., 131., 128., 134., 135., 135.; ...
      206., 201., 196., 200., 198., 197., 211., 197., 198., 200., 196., 201., 206.]);
    CXN1DE = transpose([ ...
      929., 590., 250., 375., 126., 471., 427., 471., 126., 375., 250., 590., 929.]);
    CDN1DE = transpose([ ...
      -36.,  19.,  75.,  77.,  51., 100., 105., 100.,  51.,  77.,  75.,  19., -36.]);
    P8DE = transpose([ ...
      125., 124., 124., 119., 102.,  89.,  98.,  89., 102., 119., 124., 124., 125.; ...
      147., 149., 151., 158., 164., 168., 166., 168., 164., 158., 151., 149., 147.; ...
      195., 194., 193., 195., 196., 191., 190., 191., 196., 195., 193., 194., 195.]);
    CXN8DE = transpose([ ...
      159.,  75.,  -9.,-151., 511., 694., 721., 694., 511.,-151.,  -9.,  75., 159.]);
    CDN8DE = transpose([ ...
       53.,  47.,  42., -45., 594.,1002., 733.,1002., 594., -45.,  42.,  47.,  53.]);
    P5DE = transpose([ ...
      109., 102.,  95.,  93.,  95.,  92.,  86.,  92.,  95.,  93.,  95., 102., 109.; ...
      161., 165., 169., 167., 160., 162., 164., 162., 160., 167., 169., 165., 161.; ...
      197., 200., 204., 203., 209., 214., 218., 214., 209., 203., 204., 200., 197.]);
    CXN5DE = transpose([ ...
      703., 472., 242.,-524.,-241., 364., 532., 364.,-241.,-524., 242., 472., 703.]);
    CDN5DE = transpose([ ...
      533., 416., 300.,-591.,-398., 115., 201., 115.,-398.,-591., 300., 416., 533.]);
    P3DE = transpose([ ...
       70.,  78.,  86.,  89.,  90.,  92.,  89.,  92.,  90.,  89.,  86.,  78.,  70.; ...
      159., 155., 152., 153., 152., 147., 143., 147., 152., 153., 152., 155., 159.; ...
      212., 212., 212., 208., 208., 207., 205., 207., 208., 208., 212., 212., 212.]);
    CXN3DE = transpose([  277.,   77., -123., -364., -136., ...
         317.,  411.,  317., -136., -364., -123.,   77.,  277.]);
    CDN3DE = transpose([  -24.,  -15.,   -6.,  -74., -217., ...
         188.,  269.,  188., -217.,  -74.,   -6.,  -15.,  -24.]);
    P2NE = transpose([ ...
      113., 111., 109., 107., 103., 106., 102., 106., 103., 107., 109., 111., 113.; ...
      155., 157., 160., 158., 157., 146., 137., 146., 157., 158., 160., 157., 155.; ...
      188., 195., 202., 208., 209., 207., 201., 207., 209., 208., 202., 195., 188.]);
    CXN2NE = transpose([ ...
       76., 134., 192., 821., 801., 583., 551., 583., 801., 821., 192., 134.,  76.]);
%    CDN2NE = transpose([ ...
%     -19., -83.,-148., 399., 289., 340., 542., 340., 289., 399.,-148., -83., -19.]);
    %        equator corrected  
    CDN2NE = transpose([ ...
      -19., -83.,-148., 399., 289., 340., 340., 340., 289., 399.,-148., -83., -19.]);
    P1NE = transpose([ ...
      112., 111., 109., 108., 109., 110., 109., 110., 109., 108., 109., 111., 112.; ...
      132., 132., 133., 131., 134., 143., 143., 143., 134., 131., 133., 132., 132.; ...
      220., 211., 201., 195., 196., 204., 213., 204., 196., 195., 201., 211., 220.]);
    CXN1NE = transpose([  806.,  689.,  572.,  607.,  540., ...
         535.,  443.,  535.,  540.,  607.,  572.,  689.,  806.]);
    CDN1NE = transpose([  124.,   23.,  -77.,   50.,  128., ...
         120.,   79.,  120.,  128.,   50.,  -77.,   23.,  124.]);
    P8NE = transpose([ ...
      124., 124., 124., 123., 119., 108., 110., 108., 119., 123., 124., 124., 124.; ...
      149., 149., 149., 149., 155., 164., 170., 164., 155., 149., 149., 149., 149.; ...
      195., 195., 194., 193., 194., 195., 194., 195., 194., 193., 194., 195., 195.]);
    CXN8NE = transpose([   57.,  -17.,  -90.,  -19.,  260., ...
         431.,  417.,  431.,  260.,  -19.,  -90.,  -17.,   57.]);
    CDN8NE = transpose([   18.,   -7.,  -32.,   31.,  139., ...
         320.,  351.,  320.,  139.,   31.,  -32.,   -7.,   18.]);
    P5NE = transpose([ ...
      112., 105.,  98., 100.,  98.,  91.,  90.,  91.,  98., 100.,  98., 105., 112.; ...
      162., 161., 160., 163., 166., 165., 165., 165., 166., 163., 160., 161., 162.; ...
      208., 205., 203., 201., 202., 207., 208., 207., 202., 201., 203., 205., 208.]);
    CXN5NE = transpose([  575.,  437.,  299.,  198.,  293., ...
         384.,  403.,  384.,  293.,  198.,  299.,  437.,  575.]);
    CDN5NE = transpose([  353.,  256.,  159.,   58.,  203., ...
         253.,  230.,  253.,  203.,   58.,  159.,  256.,  353.]);
    P3NE = transpose([ ...
       72.,  78.,  85.,  90.,  94.,  89.,  88.,  89.,  94.,  90.,  85.,  78.,  72.; ...
      163., 159., 154., 160., 164., 161., 157., 161., 164., 160., 154., 159., 163.; ...
      212., 211., 210., 205., 208., 224., 231., 224., 208., 205., 210., 211., 212.]);
    CXN3NE = transpose([ 1457.,  763.,   68.,  153.,  187., ...
         390.,  364.,  390.,  187.,  153.,   68.,  763., 1457.]);
    CDN3NE = transpose([  722.,  329.,  -63.,   40.,   31., ...
         102.,  111.,  102.,   31.,   40.,  -63.,  329.,  722.]);
    %      Solstice
    P2DS = transpose([ ...
       90.,  90.,  91.,  91.,  98., 105., 112., 119., 125., 124., 120., 126., 131.; ...
      155., 156., 156., 161., 163., 158., 152., 155., 154., 147., 152., 148., 145.; ...
      205., 202., 199., 191., 195., 206., 212., 210., 206., 205., 203., 201., 199.]);
    CXN2DS = transpose([ -714., -294.,  126.,  546.,  516., ...
         486.,  456.,  426.,  396.,  225.,  366.,  710., 1055.]);
    CDN2DS = transpose([ -732., -357.,   17.,  392.,  327., ...
         262.,  197.,  132.,   67.,  -80.,   62.,  330.,  598.]);
    P1DS = transpose([ ...
      105., 107., 108., 112., 111., 111., 112., 113., 113., 113., 114., 116., 118.; ...
      125., 127., 130., 132., 135., 134., 135., 139., 134., 133., 135., 136., 137.; ...
      189., 193., 198., 211., 218., 202., 199., 200., 206., 213., 217., 224., 231.]);
    CXN1DS = transpose([  706.,  480.,  255.,   56.,  310., ...
         111.,  164.,  249.,  324.,  629.,  837.,  938., 1038.]);
    CDN1DS = transpose([   25.,   51.,   77.,   26.,  107., ...
         131.,  118.,  141.,  130.,   68.,   45.,  123.,  202.]);
    P8DS = transpose([ ...
      125., 122., 120., 121., 119., 116., 113., 110., 107., 114., 125., 127., 129.; ...
      148., 147., 146., 149., 159., 163., 162., 160., 171., 160., 154., 152., 150.; ...
      199., 199., 198., 200., 198., 200., 214., 221., 215., 192., 193., 196., 200.]);
    CXN8DS = transpose([  201.,   47., -106., -130., -106., ...
          26.,  158.,  290.,  422.,  214.,  185.,  224.,  263.]);
    CDN8DS = transpose([  104.,   63.,   22.,   -3.,   -1., ...
          60.,  121.,  182.,  242.,  170.,   51.,   67.,   84.]);
    P5DS = transpose([ ...
       91.,  88.,  86.,  95.,  94.,  88.,  85.,  90.,  95.,  96.,  96.,  97.,  98.; ...
      165., 167., 168., 150., 159., 161., 161., 159., 158., 158., 145., 149., 154.; ...
      189., 191., 194., 201., 204., 200., 196., 193., 191., 196., 204., 201., 199.]);
    CXN5DS = transpose([ -1839.,-1044., -248., -253., -416., ...
          17.,  156.,  118., -110.,  372.,  934.,  895.,  856.]);
    CDN5DS = transpose([ -1603.,-1092., -581., -656., -433., ...
        -166.,   51.,  -29., -325.,  117.,  -36.,  321.,  678.]);
    P3DS = transpose([ ...
       97.,  94.,  92.,  94.,  90.,  89.,  90.,  92.,  93.,  91.,  95.,  85.,  75.; ...
      155., 148., 141., 142., 142., 140., 139., 142., 145., 141., 144., 155., 167.; ...
      197., 200., 204., 207., 200., 203., 206., 209., 212., 219., 219., 204., 190.]);
    CXN3DS = transpose([ -894., -797., -700., -604.,  139., ...
         246.,  354.,  462.,  569.,  142.,   81.,  293.,  505.]);
    CDN3DS = transpose([ -740., -577., -414., -140.,   55., ...
         -27.,  110.,   74.,  -10., -122.,   52.,  209.,  366.]);
    P2NS = transpose([ ...
      105., 107., 110., 112., 115., 110., 104.,  99.,  95., 103., 117., 127., 136.; ...
      149., 146., 144., 147., 143., 139., 138., 143., 152., 154., 154., 148., 142.; ...
      198., 200., 201., 206., 211., 210., 209., 200., 196., 197., 205., 205., 206.]);
    CXN2NS = transpose([ -817., -511., -205.,  100.,  406., ...
         312.,  217.,  123.,  640., 1037.,  580.,  677.,  773.]);
    CDN2NS = transpose([  755.,  684.,  612.,  540.,  469., ...
         191.,  -87., -365., -171., -224., -112.,  146.,  403.]);
    P1NS = transpose([ ...
      104., 108., 112., 112., 114., 113., 112., 112., 112., 114., 114., 116., 119.; ...
      137., 135., 133., 131., 136., 137., 133., 134., 131., 133., 135., 135., 134.; ...
      240., 224., 208., 207., 204., 201., 199., 208., 208., 211., 196., 198., 200.]);
    CXN1NS = transpose([ 1440.,  430., -580.,  201.,  354., ...
         470.,  281.,  329.,  511.,  546.,  741.,  911., 1082.]);
    CDN1NS = transpose([ -236., -256., -276.,  182.,  155., ...
         100.,  128.,  148.,  100.,  -61., -102.,   33.,  169.]);
    P8NS = transpose([ ...
      128., 127., 127., 127., 126., 122., 102., 115., 116., 118., 121., 121., 121.; ...
      153., 153., 153., 153., 154., 158., 163., 156., 150., 146., 147., 147., 147.; ...
      198., 197., 197., 195., 193., 194., 197., 198., 198., 198., 198., 198., 199.]);
    CXN8NS = transpose([  199.,   -8., -215.,   92.,  275., ...
         284.,  304.,  322.,  284.,  161.,  121.,  200.,  280.]);
    CDN8NS = transpose([  123.,    1., -121.,   -6.,  151., ...
         146.,  -19.,  169.,  149.,   75.,   22.,   78.,  134.]);
    P5NS = transpose([ ...
       85.,  89.,  93., 102.,  95.,  85.,  83.,  86.,  97., 101.,  93.,  92.,  90.; ...
      166., 163., 160., 167., 168., 167., 165., 167., 169., 164., 157., 162., 166.; ...
      217., 207., 196., 183., 193., 194., 197., 205., 209., 205., 205., 201., 198.]);
    CXN5NS = transpose([ ...
      274., 246., 218.,  90., 248., 337., 370., 433., 361., 461., 541., 615., 689.]);
    CDN5NS = transpose([ ...
     -153., -61.,  30.,  48., 169., 241., 279., 260., 187., 380., 316., 436., 557.]);
    P3NS = transpose([ ...
       74.,  80.,  87.,  89.,  95.,  94.,  92.,  91.,  90.,  93.,  93.,  87.,  81.; ...
      151., 146., 141., 142., 151., 158., 159., 159., 160., 155., 149., 153., 157.; ...
      221., 205., 189., 184., 180., 175., 176., 179., 191., 199., 210., 204., 198.]);
    CXN3NS = transpose([ ...
       68.,  34.,   1., 154., 151., 178., 224., 198., 220., 380., 559., 228.,-104.]);
    CDN3NS = transpose([ ...
       97.,   1., -95.,  47.,  37., 112.,  96., 106., 129., 252., 470., 220., -30.]);
  end
  P2DEI = zeros(NJ,1);
  P1DEI = zeros(NJ,1);
  P8DEI = zeros(NJ,1);
  P5DEI = zeros(NJ,1);
  P3DEI = zeros(NJ,1);
  P2NEI = zeros(NJ,1);
  P1NEI = zeros(NJ,1);
  P8NEI = zeros(NJ,1);
  P5NEI = zeros(NJ,1);
  P3NEI = zeros(NJ,1);
  P2DSI = zeros(NJ,1);
  P1DSI = zeros(NJ,1);
  P8DSI = zeros(NJ,1);
  P5DSI = zeros(NJ,1);
  P3DSI = zeros(NJ,1);
  P2NSI = zeros(NJ,1);
  P1NSI = zeros(NJ,1);
  P8NSI = zeros(NJ,1);
  P5NSI = zeros(NJ,1);
  P3NSI = zeros(NJ,1);

  TXN2DE = CXN2DE;
  TXN1DE = CXN1DE;
  TXN8DE = CXN8DE;
  TXN5DE = CXN5DE;
  TXN3DE = CXN3DE;
  TDN2DE = CDN2DE;
  TDN1DE = CDN1DE;
  TDN8DE = CDN8DE;
  TDN5DE = CDN5DE;
  TDN3DE = CDN3DE;
  TXN2NE = CXN2NE;
  TXN1NE = CXN1NE;
  TXN8NE = CXN8NE;
  TXN5NE = CXN5NE;
  TXN3NE = CXN3NE;
  TDN2NE = CDN2NE;
  TDN1NE = CDN1NE;
  TDN8NE = CDN8NE;
  TDN5NE = CDN5NE;
  TDN3NE = CDN3NE;
  TXN2DS = CXN2DS;
  TXN1DS = CXN1DS;
  TXN8DS = CXN8DS;
  TXN5DS = CXN5DS;
  TXN3DS = CXN3DS;
  TDN2DS = CDN2DS;
  TDN1DS = CDN1DS;
  TDN8DS = CDN8DS;
  TDN5DS = CDN5DS;
  TDN3DS = CDN3DS;
  TXN2NS = CXN2NS;
  TXN1NS = CXN1NS;
  TXN8NS = CXN8NS;
  TXN5NS = CXN5NS;
  TXN3NS = CXN3NS;
  TDN2NS = CDN2NS;
  TDN1NS = CDN1NS;
  TDN8NS = CDN8NS;
  TDN5NS = CDN5NS;
  TDN3NS = CDN3NS;
  if (((DDD >= 265) && (DDD < 354)) || ...
      ((DDD >= 354) || (DDD < 79)))
    TXN2DS = IRI2012.SWAPEL(NI,TXN2DS);
    TDN2DS = IRI2012.SWAPEL(NI,TDN2DS);
    TXN1DS = IRI2012.SWAPEL(NI,TXN1DS);
    TDN1DS = IRI2012.SWAPEL(NI,TDN1DS);
    TXN8DS = IRI2012.SWAPEL(NI,TXN8DS);
    TDN8DS = IRI2012.SWAPEL(NI,TDN8DS);
    TXN5DS = IRI2012.SWAPEL(NI,TXN5DS);
    TDN5DS = IRI2012.SWAPEL(NI,TDN5DS);
    TXN3DS = IRI2012.SWAPEL(NI,TXN3DS);
    TDN3DS = IRI2012.SWAPEL(NI,TDN3DS);
    TXN2NS = IRI2012.SWAPEL(NI,TXN2NS);
    TDN2NS = IRI2012.SWAPEL(NI,TDN2NS);
    TXN1NS = IRI2012.SWAPEL(NI,TXN1NS);
    TDN1NS = IRI2012.SWAPEL(NI,TDN1NS);
    TXN8NS = IRI2012.SWAPEL(NI,TXN8NS);
    TDN8NS = IRI2012.SWAPEL(NI,TDN8NS);
    TXN5NS = IRI2012.SWAPEL(NI,TXN5NS);
    TDN5NS = IRI2012.SWAPEL(NI,TDN5NS);
    TXN3NS = IRI2012.SWAPEL(NI,TXN3NS);
    TDN3NS = IRI2012.SWAPEL(NI,TDN3NS);
  end
       
  %      interpolated Te values for invdip
  %      Te max-min day equinox
  XN2DEI=IRI2012.INTERP(NI,0,TXN2DE,1,INVDPQ,INVDIP);
  XN1DEI=IRI2012.INTERP(NI,0,TXN1DE,1,INVDPQ,INVDIP);
  XN8DEI=IRI2012.INTERP(NI,0,TXN8DE,1,INVDPQ,INVDIP);
  XN5DEI=IRI2012.INTERP(NI,0,TXN5DE,1,INVDPQ,INVDIP);
  XN3DEI=IRI2012.INTERP(NI,0,TXN3DE,1,INVDPQ,INVDIP);
  %      Te max-min night equinox
  XN2NEI=IRI2012.INTERP(NI,0,TXN2NE,1,INVDPQ,INVDIP);
  XN1NEI=IRI2012.INTERP(NI,0,TXN1NE,1,INVDPQ,INVDIP);
  XN8NEI=IRI2012.INTERP(NI,0,TXN8NE,1,INVDPQ,INVDIP);
  XN5NEI=IRI2012.INTERP(NI,0,TXN5NE,1,INVDPQ,INVDIP);
  XN3NEI=IRI2012.INTERP(NI,0,TXN3NE,1,INVDPQ,INVDIP);
  %      Te med-min day equinox
  DN2DEI=IRI2012.INTERP(NI,0,TDN2DE,1,INVDPQ,INVDIP);
  DN1DEI=IRI2012.INTERP(NI,0,TDN1DE,1,INVDPQ,INVDIP);
  DN8DEI=IRI2012.INTERP(NI,0,TDN8DE,1,INVDPQ,INVDIP);
  DN5DEI=IRI2012.INTERP(NI,0,TDN5DE,1,INVDPQ,INVDIP);
  DN3DEI=IRI2012.INTERP(NI,0,TDN3DE,1,INVDPQ,INVDIP);
  %      Te med-min night equinox
  DN2NEI=IRI2012.INTERP(NI,0,TDN2NE,1,INVDPQ,INVDIP);
  DN1NEI=IRI2012.INTERP(NI,0,TDN1NE,1,INVDPQ,INVDIP);
  DN8NEI=IRI2012.INTERP(NI,0,TDN8NE,1,INVDPQ,INVDIP);
  DN5NEI=IRI2012.INTERP(NI,0,TDN5NE,1,INVDPQ,INVDIP);
  DN3NEI=IRI2012.INTERP(NI,0,TDN3NE,1,INVDPQ,INVDIP);
  %
  %      Te max-min day solstice
  XN2DSI=IRI2012.INTERP(NI,0,TXN2DS,1,INVDPQ,INVDIP);
  XN1DSI=IRI2012.INTERP(NI,0,TXN1DS,1,INVDPQ,INVDIP);
  XN8DSI=IRI2012.INTERP(NI,0,TXN8DS,1,INVDPQ,INVDIP);
  XN5DSI=IRI2012.INTERP(NI,0,TXN5DS,1,INVDPQ,INVDIP);
  XN3DSI=IRI2012.INTERP(NI,0,TXN3DS,1,INVDPQ,INVDIP);
  %      Te max-min night solstice
  XN2NSI=IRI2012.INTERP(NI,0,TXN2NS,1,INVDPQ,INVDIP);
  XN1NSI=IRI2012.INTERP(NI,0,TXN1NS,1,INVDPQ,INVDIP);
  XN8NSI=IRI2012.INTERP(NI,0,TXN8NS,1,INVDPQ,INVDIP);
  XN5NSI=IRI2012.INTERP(NI,0,TXN5NS,1,INVDPQ,INVDIP);
  XN3NSI=IRI2012.INTERP(NI,0,TXN3NS,1,INVDPQ,INVDIP);
  %      Te med-min day solstice
  DN2DSI=IRI2012.INTERP(NI,0,TDN2DS,1,INVDPQ,INVDIP);
  DN1DSI=IRI2012.INTERP(NI,0,TDN1DS,1,INVDPQ,INVDIP);
  DN8DSI=IRI2012.INTERP(NI,0,TDN8DS,1,INVDPQ,INVDIP);
  DN5DSI=IRI2012.INTERP(NI,0,TDN5DS,1,INVDPQ,INVDIP);
  DN3DSI=IRI2012.INTERP(NI,0,TDN3DS,1,INVDPQ,INVDIP);
  %      Te med-min night solstice
  DN2NSI=IRI2012.INTERP(NI,0,TDN2NS,1,INVDPQ,INVDIP);
  DN1NSI=IRI2012.INTERP(NI,0,TDN1NS,1,INVDPQ,INVDIP);
  DN8NSI=IRI2012.INTERP(NI,0,TDN8NS,1,INVDPQ,INVDIP);
  DN5NSI=IRI2012.INTERP(NI,0,TDN5NS,1,INVDPQ,INVDIP);
  DN3NSI=IRI2012.INTERP(NI,0,TDN3NS,1,INVDPQ,INVDIP);
  for I=1:NJ
    P2DEI(I)=IRI2012.INTERP(NI,1,P2DE,I,INVDPQ,INVDIP);
    P2NEI(I)=IRI2012.INTERP(NI,1,P2NE,I,INVDPQ,INVDIP);
    P1DEI(I)=IRI2012.INTERP(NI,1,P1DE,I,INVDPQ,INVDIP);
    P1NEI(I)=IRI2012.INTERP(NI,1,P1NE,I,INVDPQ,INVDIP);
    P8DEI(I)=IRI2012.INTERP(NI,1,P8DE,I,INVDPQ,INVDIP);
    P8NEI(I)=IRI2012.INTERP(NI,1,P8NE,I,INVDPQ,INVDIP);
    P5DEI(I)=IRI2012.INTERP(NI,1,P5DE,I,INVDPQ,INVDIP);
    P5NEI(I)=IRI2012.INTERP(NI,1,P5NE,I,INVDPQ,INVDIP);
    P3DEI(I)=IRI2012.INTERP(NI,1,P3DE,I,INVDPQ,INVDIP);
    P3NEI(I)=IRI2012.INTERP(NI,1,P3NE,I,INVDPQ,INVDIP);
    
    P2DSI(I)=IRI2012.INTERP(NI,1,P2DS,I,INVDPQ,INVDIP);
    P2NSI(I)=IRI2012.INTERP(NI,1,P2NS,I,INVDPQ,INVDIP);
    P1DSI(I)=IRI2012.INTERP(NI,1,P1DS,I,INVDPQ,INVDIP);
    P1NSI(I)=IRI2012.INTERP(NI,1,P1NS,I,INVDPQ,INVDIP);
    P8DSI(I)=IRI2012.INTERP(NI,1,P8DS,I,INVDPQ,INVDIP);
    P8NSI(I)=IRI2012.INTERP(NI,1,P8NS,I,INVDPQ,INVDIP);
    P5DSI(I)=IRI2012.INTERP(NI,1,P5DS,I,INVDPQ,INVDIP);
    P5NSI(I)=IRI2012.INTERP(NI,1,P5NS,I,INVDPQ,INVDIP);
    P3DSI(I)=IRI2012.INTERP(NI,1,P3DS,I,INVDPQ,INVDIP);
    P3NSI(I)=IRI2012.INTERP(NI,1,P3NS,I,INVDPQ,INVDIP);
  end
  MLTTMP=MLT-1;
  if MLTTMP < 0
    MLTTMP=MLTTMP+24.0;
  end
  MLTRAD=MLTTMP*IRI2012.humr;
  if (((DDD >= 79) && (DDD < 171)) || ...
      ((DDD >= 265) && (DDD < 354)))
    %      Equinox
    TP200A = IRI2012.TPCAS(MLTRAD,PF107,P2000A,XN2DEI,DN2DEI,P2DEI,XN2NEI,DN2NEI,P2NEI);
    TP140A = IRI2012.TPCAS(MLTRAD,PF107,P1400A,XN1DEI,DN1DEI,P1DEI,XN1NEI,DN1NEI,P1NEI);
    TP850A = IRI2012.TPCAS(MLTRAD,PF107,P850A,XN8DEI,DN8DEI,P8DEI,XN8NEI,DN8NEI,P8NEI);
    TP550A = IRI2012.TPCAS(MLTRAD,PF107,P550A,XN5DEI,DN5DEI,P5DEI,XN5NEI,DN5NEI,P5NEI);
    TP350A = IRI2012.TPCAS(MLTRAD,PF107,P350A,XN3DEI,DN3DEI,P3DEI,XN3NEI,DN3NEI,P3NEI);
    %       Solstice
    TP200B = IRI2012.TPCAS(MLTRAD,PF107,P2000B,XN2DSI,DN2DSI,P2DSI,XN2NSI,DN2NSI,P2NSI);
    TP140B = IRI2012.TPCAS(MLTRAD,PF107,P1400B,XN1DSI,DN1DSI,P1DSI,XN1NSI,DN1NSI,P1NSI);
    TP850B = IRI2012.TPCAS(MLTRAD,PF107,P850B,XN8DSI,DN8DSI,P8DSI,XN8NSI,DN8NSI,P8NSI);
    TP550B = IRI2012.TPCAS(MLTRAD,PF107,P550B,XN5DSI,DN5DSI,P5DSI,XN5NSI,DN5NSI,P5NSI);
    TP350B = IRI2012.TPCAS(MLTRAD,PF107,P350B,XN3DSI,DN3DSI,P3DSI,XN3NSI,DN3NSI,P3NSI);
  else%if (((DDD >= 171) && (DDD < 265)) || ...
      %    ((DDD >= 354) || (DDD < 79)))
    %       Solstice
    TP200A = IRI2012.TPCAS(MLTRAD,PF107,P2000A,XN2DSI,DN2DSI,P2DSI,XN2NSI,DN2NSI,P2NSI);
    TP140A = IRI2012.TPCAS(MLTRAD,PF107,P1400A,XN1DSI,DN1DSI,P1DSI,XN1NSI,DN1NSI,P1NSI);
    TP850A = IRI2012.TPCAS(MLTRAD,PF107,P850A,XN8DSI,DN8DSI,P8DSI,XN8NSI,DN8NSI,P8NSI);
    TP550A = IRI2012.TPCAS(MLTRAD,PF107,P550A,XN5DSI,DN5DSI,P5DSI,XN5NSI,DN5NSI,P5NSI);
    TP350A = IRI2012.TPCAS(MLTRAD,PF107,P350A,XN3DSI,DN3DSI,P3DSI,XN3NSI,DN3NSI,P3NSI);
    %      Equinox
    TP200B = IRI2012.TPCAS(MLTRAD,PF107,P2000B,XN2DEI,DN2DEI,P2DEI,XN2NEI,DN2NEI,P2NEI);
    TP140B = IRI2012.TPCAS(MLTRAD,PF107,P1400B,XN1DEI,DN1DEI,P1DEI,XN1NEI,DN1NEI,P1NEI);
    TP850B = IRI2012.TPCAS(MLTRAD,PF107,P850B,XN8DEI,DN8DEI,P8DEI,XN8NEI,DN8NEI,P8NEI);
    TP550B = IRI2012.TPCAS(MLTRAD,PF107,P550B,XN5DEI,DN5DEI,P5DEI,XN5NEI,DN5NEI,P5NEI);
    TP350B = IRI2012.TPCAS(MLTRAD,PF107,P350B,XN3DEI,DN3DEI,P3DEI,XN3NEI,DN3NEI,P3NEI);
  end

end

