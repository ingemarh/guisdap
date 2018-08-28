classdef OEIS
%OEIS Offline Encyclopedia of Integer Sequences
% Copyright(c) 2014 by Jonathan Kipling Knight (<a href="matlab:
% web('mailto:drkipknight@aol.com')">drkipknight@aol.com</a>)
% For more information, see the online <a href="matlab:
% web('http://oeis.org')">OEIS</a>.

  properties (Constant)
    % Decimal expansion of pi
    % pi = 4 sum_{n=0}^infty frac{(-1)^{n+1}}{2 n-1}
    % (Formerly M2218 N0880)
    % see <a href="matlab:web('http://oeis.org/A000796')">A000796</a>
    A000796 = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214e0;
    % Decimal expansion of pi
    Pi = OEIS.A000796;
    % Decimal expansion of 2 pi.
    % see <a href="matlab:web('http://oeis.org/A019692')">A019692</a>
    A019692 = 6.28318530717958647692528676655900576839433879875021164194988918461563281257241799725606965068423413e0;
    % Decimal expansion of 2 pi.
    TwoPi = OEIS.A019692;
    % Decimal expansion of pi/180.
    % see <a href="matlab:web('http://oeis.org/A019685')">A019685</a>
    A019685 = 1.74532925199432957692369076848861271344287188854172545609719144017100911460344944368224156963450948e-2;
    % Decimal expansion of pi/180.
    PiOver180 = OEIS.A019685;
    % Decimal expansion of pi/12.
    % see <a href="matlab:web('http://oeis.org/A019679')">A019679</a>
    A019679 = 0.261799387799149436538553615273291907016430783281258818414578716025651367190517416552336235445176422e0;
    % Decimal expansion of pi/12.
    PiOver12 = OEIS.A019679;
    % Decimal expansion of Napier's number
    % see <a href="matlab:web('http://oeis.org/A001113')">A001113</a>
    % e = sum(n=0 to infty) 1/n!
    % (Formerly M1727 N0684)
    A001113 = 2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746e0;
    % Decimal expansion of Napier's number
    e = OEIS.A001113;
    % Decimal expansion of Euler's constant (or Euler-Mascheroni constant)
    % see <a href="matlab:web('http://oeis.org/A001620')">A001620</a>
    % gamma = sum(n=1 to infty) 1/n - ln(1+1/n)
    % (Formerly M3755 N1532)
    A001620 = 5.77215664901532860606512090082402431042159335939923598805767234884867726777664670936947063291746749e-1;
    % Decimal expansion of Euler's constant (or Euler-Mascheroni constant)
    EulerMascheroni = OEIS.A001620;
    % Decimal expansion of Gamma(1/3)
    % see <a href="matlab:web('http://oeis.org/A073005')">A073005</a>
    % Gamma(1/3) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A073005 = 2.67893853470774763365569294097467764412868937795730110095042832759041761016774381954098288904118878941915e0;
    % Decimal expansion of Gamma(1/3)
    gammaOneThird = OEIS.A073005;
    % Decimal expansion of Gamma(2/3)
    % see <a href="matlab:web('http://oeis.org/A073006')">A073006</a>
    % Gamma(2/3) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A073006 = 1.35411793942640041694528802815451378551932726605679369839402246796378296540174254167583414795297291110643e0;
    % Decimal expansion of Gamma(2/3)
    gammaTwoThirds = OEIS.A073006;
    % Decimal expansion of Gamma(4/3)
    % see <a href="matlab:web('http://oeis.org/A202623')">A202623</a>
    % Gamma(4/3) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A202623 = 8.929795115692492112185643136582258813762892979511569249211218564313658225881376229792652433700316809442530139203389247939846994296347062929e-1;
    % Decimal expansion of Gamma(4/3)
    gammaFourThirds = OEIS.A202623;
    % Decimal expansion of Gamma(5/3)
    % see <a href="matlab:web('http://oeis.org/A203129')">A203129</a>
    % Gamma(5/3) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A203129 = 9.02745292950933611296858685436342523679551510704529132262681645309188643601161694450556098635315274e-1;
    % Decimal expansion of Gamma(5/3)
    gammaFiveThirds = OEIS.A203129;
    % Decimal expansion of Gamma(1/4)
    % see <a href="matlab:web('http://oeis.org/A068466')">A068466</a>
    % Gamma(1/4) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A068466 = 3.62560990822190831193068515586767200299516768288006546743337799956991924353872912161836013672338430036147e0;
    % Decimal expansion of Gamma(1/4)
    gammaOneFourth = OEIS.A068466;
    % Decimal expansion of Gamma(3/4)
    % see <a href="matlab:web('http://oeis.org/A068465')">A068465</a>
    % Gamma(3/4) = sqrt(2)pi/Gamma(1/4)
    % where Gamma is the Euler gamma function.
    A068465 = 1.22541670246517764512909830336289052685123924810807061123011893828982288842679835723717237621491506658217e0;
    % Decimal expansion of Gamma(3/4)
    gammaThreeFourths = OEIS.A068465;
    % Decimal expansion of Gamma(5/4)
    % see <a href="matlab:web('http://oeis.org/A068467')">A068467</a>
    % Gamma(5/4) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A068467 = 9.0640247705547707798267128896691800074879192072001636685834449989247981088468228040459003418084607509036e-1;
    % Decimal expansion of Gamma(5/4)
    gammaFiveFourths = OEIS.A068467;
    % Decimal expansion of Gamma(7/4)
    % see <a href="matlab:web('http://oeis.org/A203130')">A203130</a>
    % Gamma(7/4) = sum(n=0 to infty) a(n)*10^n
    % where Gamma is the Euler gamma function.
    A203130 = 9.19062526848883233846823727522167895138429436081052958422589203717367166320098767927879282161186299e-1;
    % Decimal expansion of Gamma(7/4)
    gammaSevenFourths = OEIS.A203130;
    % matrix of Gamma(i/j) i=one through ten, j=one through four
    gamma = transpose([1,1,2,6,24,120,720,5040,40320,362880; ...
             OEIS.sqrtPi,1,(1/2)*OEIS.sqrtPi,1,(1*3/2^2)*OEIS.sqrtPi,2,(1*3*5/2^3)*OEIS.sqrtPi,6,(1*3*5*7/2^4)*OEIS.sqrtPi,24; ...
             OEIS.gammaOneThird,OEIS.gammaTwoThirds,1,OEIS.gammaFourThirds,OEIS.gammaFiveThirds,1,(4/3)*OEIS.gammaFourThirds,(5/3)*OEIS.gammaFiveThirds,2,(7/3)*(4/3)*OEIS.gammaFourThirds; ...
             OEIS.gammaOneFourth,OEIS.sqrtPi,OEIS.gammaThreeFourths,1,OEIS.gammaFiveFourths,(1/2)*OEIS.sqrtPi,OEIS.gammaSevenFourths,1,(5/4)*OEIS.gammaFiveFourths,(1*3/2^2)*OEIS.sqrtPi]);
    % Decimal expansion of Riemann's Zeta(3)
    % see <a href="matlab:web('http://oeis.org/A002117')">A002117</a>
    % zeta(3) = sum(m=1 to infty) 1/m^3
    % (Formerly M0020)
    A002117 = 1.20205690315959428539973816151144999076498629234049888179227155534183820578631309018645587360933525814619915e0;
    % Decimal expansion of Riemann's Zeta(3)
    zeta3 = OEIS.A002117;
    % Decimal expansion of Riemann's Zeta(4)
    % see <a href="matlab:web('http://oeis.org/A013662')">A013662</a>
    % zeta(4) = sum(m=1 to infty) 1/m^4
    A013662 = 1.08232323371113819151600369654116790277475095191872690768297621544412061618696884655690963594169991e0;
    % Decimal expansion of Riemann's Zeta(4)
    zeta4 = OEIS.A013662;
    %zeta4 = OEIS.Pi^4/90.0;
    
    % Decimal expansion of Riemann's Zeta(5)
    % see <a href="matlab:web('http://oeis.org/A013663')">A013663</a>
    % zeta(5) = sum(m=1 to infty) 1/m^5
    A013663 = 1.03692775514336992633136548645703416805708091950191281197419267790380358978628148456004310655713333e0;
    % Decimal expansion of Riemann's Zeta(5)
    zeta5 = OEIS.A013663;
    % Decimal expansion of Riemann's Zeta(7)
    % see <a href="matlab:web('http://oeis.org/A013665')">A013665</a>
    % zeta(7) = sum(m=1 to infty) 1/m^7
    A013665 = 1.00834927738192282683979754984979675959986356056523870641728313657160147831735573534609696891385132e0;
    % Decimal expansion of Riemann's Zeta(7)
    zeta7 = OEIS.A013665;
    % Decimal expansion of Riemann's Zeta(9)
    % see <a href="matlab:web('http://oeis.org/A013667')">A013667</a>
    % zeta(9) = sum(m=1 to infty) 1/m^9
    A013667 = 1.00200839282608221441785276923241206048560585139488875654859661590978505339025839895039306912716958e0;
    % Decimal expansion of Riemann's Zeta(9)
    zeta9 = OEIS.A013667;
    % Decimal expansion of Riemann's Zeta(2)
    zeta2 = OEIS.Pi^2/6.0;
    % Decimal expansion of Riemann's Zeta(6)
    zeta6 = OEIS.Pi^6/945.0;
    % Decimal expansion of Riemann's Zeta(8)
    zeta8 = OEIS.Pi^8/9450.0;
    % Decimal expansion of Riemann's Zeta(10)
    zeta10 = OEIS.Pi^10/93555.0;
    % array of Riemann's Zeta(i) one through ten
    zeta = [Inf,OEIS.zeta2,OEIS.zeta3,OEIS.zeta4,OEIS.zeta5,OEIS.zeta6,OEIS.zeta7,OEIS.zeta8,OEIS.zeta9,OEIS.zeta10];
    % Decimal expansion of positive solution to 5 (1-e^u) + u e^u = 0
    % see <a href="matlab:web('http://oeis.org/A094090')">A094090</a>
    % u = 5+sum(n=1 to infty) (-1)^(n-1)n^(n-2)/((n-1)!) (-5/exp(5))^n
    % Mathematica solution:
    % N[5+ProductLog[-5/Exp[5]],40]
    A094090 = 4.96511423174427630369875913132289394405558498679725097281444614478046398795745297223827045066000960829776e0;
    % Decimal expansion of natural logarithm of 2.
    % see <a href="matlab:web('http://oeis.org/A002162')">A002162</a>
    % (Formerly M4074 N1689)
    A002162 = 6.93147180559945309417232121458176568075500134360255254120680009493393621969694715605863326996418687e-1;
    % Decimal expansion of natural logarithm of 2.
    Log2 = OEIS.A002162;
    % Decimal expansion of natural logarithm of 3.
    % see <a href="matlab:web('http://oeis.org/A002391')">A002391</a>
    % (Formerly M4595 N1960)
    A002391 = 1.09861228866810969139524523692252570464749055782274945173469433363749429321860896687361575481373208878797e0;
    % Decimal expansion of natural logarithm of 3.
    Log3 = OEIS.A002391;
    % Decimal expansion of natural log of Pi.
    % see <a href="matlab:web('http://oeis.org/A053510')">A053510</a>
    A053510 = 1.144729885849400174143427351353058711647294812915311571513623071472137769884826079783623270275489707702009e0;
    % Decimal expansion of natural log of Pi.
    LogPi = OEIS.A053510;
    % Decimal expansion of natural log of 7.
    % see <a href="matlab:web('http://oeis.org/A016630')">A016630</a>
    A016630 = 1.94591014905531330510535274344317972963708472958186118845939014993757986275206926778765849858787152e0;
    % Decimal expansion of natural log of 7.
    Log7 = OEIS.A016630;
    % Decimal expansion of natural logarithm of 10.
    % see <a href="matlab:web('http://oeis.org/A002392')">A002392</a>
    % (Formerly M0394 N0151)
    A002392 = 2.30258509299404568401799145468436420760110148862877297603332790096757e0;
    % Decimal expansion of natural logarithm of 10.
    Log10 = OEIS.A002392;
    % matrix of log(i/j) i=one through ten, j=one through four
    Log = transpose([0,OEIS.Log2,OEIS.Log3,2*OEIS.Log2,OEIS.Log10-OEIS.Log2,OEIS.Log2+OEIS.Log3,OEIS.Log7,3*OEIS.Log2,2*OEIS.Log3,OEIS.Log10; ...
      -OEIS.Log2,0,OEIS.Log3-OEIS.Log2,OEIS.Log2,OEIS.Log10-2*OEIS.Log2,OEIS.Log3,OEIS.Log7-OEIS.Log2,2*OEIS.Log2,2*OEIS.Log3-OEIS.Log2,OEIS.Log10-OEIS.Log2; ...
      -OEIS.Log3,OEIS.Log2-OEIS.Log3,0,2*OEIS.Log2-OEIS.Log3,OEIS.Log10-OEIS.Log2-OEIS.Log3,OEIS.Log2,OEIS.Log7-OEIS.Log3,3*OEIS.Log2-OEIS.Log3,OEIS.Log3,OEIS.Log10-OEIS.Log3; ...
      -2*OEIS.Log2,-OEIS.Log2,OEIS.Log3-2*OEIS.Log2,0,OEIS.Log10-3*OEIS.Log2,OEIS.Log3-OEIS.Log2,OEIS.Log7-2*OEIS.Log2,OEIS.Log2,2*OEIS.Log3-2*OEIS.Log2,OEIS.Log10-2*OEIS.Log2]);
    % Decimal expansion of the golden ratio.
    % see <a href="matlab:web('http://oeis.org/A001622')">A001622</a>
    % phi = tau = (1 + sqrt(5))/2
    % phi = 1+1/phi
    % (Formerly M4046 N1679)
    A001622 = 1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475e0;
    % Decimal expansion of the golden ratio.
    GoldenRatio = OEIS.A001622;
    % Decimal expansion of cube root of 2.
    % see <a href="matlab:web('http://oeis.org/A002580')">A002580</a>
    % (Formerly M1354 N0521)
    A002580 = 1.25992104989487316476721060727822835057025146470150798008197511215529967651395948372939656243625509415431025e0;
    % Decimal expansion of cube root of 2.
    cbrt2 = OEIS.A002580;
    % Decimal expansion of cube root of 3.
    % see <a href="matlab:web('http://oeis.org/A002581')">A002581</a>
    % (Formerly M3220 N1304)
    A002581 = 1.44224957030740838232163831078010958839186925349935057754641619454168759682999733985475547970564525668683508e0;
    % Decimal expansion of cube root of 3.
    cbrt3 = OEIS.A002581;
    % Decimal expansion of cube root of 5.
    % see <a href="matlab:web('http://oeis.org/A005481')">A005481</a>
    % (Formerly M4319)
    A005481 = 1.70997594667669698935310887254386010986805511054305492438286170744429592050417321625718701002018900220450e0;
    % Decimal expansion of cube root of 5.
    cbrt5 = OEIS.A005481;
    % Decimal expansion of cube root of 7.
    % see <a href="matlab:web('http://oeis.org/A005482')">A005482</a>
    % (Formerly M4592)
    A005482 = 1.91293118277238910119911683954876028286243905034587576621064764044723427617923075600752544147728570990454e0;
    % Decimal expansion of cube root of 7.
    cbrt7 = OEIS.A005482;
    % matrix of cbrt(i/j) i=one through ten, j=one through four
    cbrt = transpose([1,OEIS.cbrt2,OEIS.cbrt3,OEIS.cbrt2^2,OEIS.cbrt5,OEIS.cbrt2*OEIS.cbrt3,OEIS.cbrt7,2,OEIS.cbrt3^2,OEIS.cbrt5*OEIS.cbrt2; ...
      1/OEIS.cbrt2,1,OEIS.cbrt3/OEIS.cbrt2,OEIS.cbrt2,OEIS.cbrt5/OEIS.cbrt2,OEIS.cbrt3,OEIS.cbrt7/OEIS.cbrt2,2/OEIS.cbrt2,OEIS.cbrt3^2/OEIS.cbrt2,OEIS.cbrt5; ...
      1/OEIS.cbrt3,OEIS.cbrt2/OEIS.cbrt3,1,OEIS.cbrt2^2/OEIS.cbrt3,OEIS.cbrt5/OEIS.cbrt3,OEIS.cbrt2,OEIS.cbrt7/OEIS.cbrt3,2/OEIS.cbrt3,OEIS.cbrt3,OEIS.cbrt5*OEIS.cbrt2/OEIS.cbrt3; ...
      1/OEIS.cbrt2^2,1/OEIS.cbrt2,OEIS.cbrt3/OEIS.cbrt2^2,1,OEIS.cbrt5/OEIS.cbrt2^2,OEIS.cbrt3/OEIS.cbrt2,OEIS.cbrt7/OEIS.cbrt2^2,OEIS.cbrt2,OEIS.cbrt3^2/OEIS.cbrt2^2,OEIS.cbrt5/OEIS.cbrt2]);
    % Decimal expansion of square root of 2.
    % see <a href="matlab:web('http://oeis.org/A002193')">A002193</a>
    % (Formerly M3195 N1291)
    A002193 = 1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157e0;
    % Decimal expansion of square root of 2.
    sqrt2 = OEIS.A002193;
    % Decimal expansion of square root of 3.
    % see <a href="matlab:web('http://oeis.org/A002194')">A002194</a>
    % (Formerly M4326 N1812)
    A002194 = 1.7320508075688772935274463415058723669428052538103806280558069794519330169088000370811461867572485756756261414154e0;
    % Decimal expansion of square root of 3.
    sqrt3 = OEIS.A002194;
    % Decimal expansion of square root of 5.
    % see <a href="matlab:web('http://oeis.org/A002163')">A002163</a>
    % (Formerly M0293 N0105)
    A002163 = 2.23606797749978969640917366873127623544061835961152572427089724541052092563780489941441440837878227e0;
    % Decimal expansion of square root of 5.
    sqrt5 = OEIS.A002163;
    % Decimal expansion of square root of 7.
    % see <a href="matlab:web('http://oeis.org/A010465')">A010465</a>
    A010465 = 2.64575131106459059050161575363926042571025918308245018036833445920106882323028362776039288647454361e0;
    % Decimal expansion of square root of 7.
    sqrt7 = OEIS.A010465;
    % matrix of sqrt(i/j) i=one through ten, j=one through four
    sqrt = transpose([1,OEIS.sqrt2,OEIS.sqrt3,2,OEIS.sqrt5,OEIS.sqrt2*OEIS.sqrt3,OEIS.sqrt7,2*OEIS.sqrt2,3,OEIS.sqrt5*OEIS.sqrt2; ...
      1/OEIS.sqrt2,1,OEIS.sqrt3/OEIS.sqrt2,OEIS.sqrt2,OEIS.sqrt5/OEIS.sqrt2,OEIS.sqrt3,OEIS.sqrt7/OEIS.sqrt2,OEIS.sqrt2,3/OEIS.sqrt2,OEIS.sqrt5; ...
      1/OEIS.sqrt3,OEIS.sqrt2/OEIS.sqrt3,1,2/OEIS.sqrt3,OEIS.sqrt5/OEIS.sqrt3,OEIS.sqrt2,OEIS.sqrt7/OEIS.sqrt3,2*OEIS.sqrt2/OEIS.sqrt3,OEIS.sqrt3,OEIS.sqrt5*OEIS.sqrt2/OEIS.sqrt3; ...
      1/2,1/OEIS.sqrt2,OEIS.sqrt3/2,1,OEIS.sqrt5/2,OEIS.sqrt3/OEIS.sqrt2,OEIS.sqrt7/2,OEIS.sqrt2,3/2,OEIS.sqrt5/OEIS.sqrt2]);
    % Decimal expansion of square root of Pi.
    % see <a href="matlab:web('http://oeis.org/A002161')">A002161</a>
    % (Formerly M4332 N1814)
    A002161 = 1.772453850905516027298167483341145182797549456122387128213807789852911284591032181374950656738544665e0;
    % Decimal expansion of square root of Pi.
    sqrtPi = OEIS.A002161;
  end
  properties (SetAccess = private, GetAccess = private)
    % map for storing newly retrieved OEIS float values
    map = containers.Map('KeyType','uint32','ValueType','double');
  end
  methods
    function oeis = OEIS( n )
      % constructor with ability to retrieve known list of sequences
      if nargin == 1
        for i=1:length(n)
          [~,oeis] = oeis.retrieveFloat( n(i), 10.0 );
        end
      end
    end
    function [val,oeis] = retrieveFloat( oeis, n, base )
      % retrieveFloat gets a new floating point value from the online database.
      % This retrieval relies on the idea that the sequence is a decimal expansion
      % INPUT:
      % oeis - OEIS context to store retrievals
      % n - integer sequence identification integer
      % base - base of sequence
      % OUTPUT:
      % val - decimal equivalent of integer sequence
      % oeis - OEIS context with stored retrieval
      if oeis.map.isKey(n)
        val = oeis.map(n);
      else
        if nargin < 3
          base = 10.0;
        end
        [a,decEx] = OEIS.retrieveSequence( n );
        if isempty(a) % not found
          val = 0.0;
          return;
        end
        val = 0.0;
        exp = -1;
        for j=1:length(a)
          val = val + double(a(j)) * base^exp;
          exp = exp - 1;
        end
        val = val * base^double(decEx);
        oeis.map(n) = val;
      end
    end
  end
  methods (Static)
    function [a,decEx,name] = retrieveSequence( n )
      % retrieveSequence gets a new sequence from the online database.
      % INPUT:
      % n - integer sequence identification integer
      % OUTPUT:
      % a - decimal equivalent of integer sequence
      % decEx - decimal exponent
      % name - name of sequence
      type = 'int32';
      url = sprintf('http://oeis.org/search?q=id:A%06d&fmt=text',n);
      f = urlread(url);
      lns = textscan(f,'%s','Delimiter','\n');
      startline = 6;
      firstset = length(lns{1})-startline - 2;
      if firstset < 0 % not found
        a = cell.empty();
        return;
      end
      a = zeros(0,1,type);
      decEx = cast(0,type);
      k = 1;
      noMoreDigits = false;
      for i=1:firstset
        sc = textscan(lns{1}{startline+i},'%s','delimiter',',');
        lntype = sc{1}{1}(2:2);
        if lntype >= 'S' && noMoreDigits == false
          a(k) = str2double(sc{1}{1}(12:end));
          k = k + 1;
          for j=2:length(sc{1})
            if ~isempty(sc{1}{j})
              a(k) = str2double(sc{1}{j});
              k = k + 1;
            else
              break;
            end
          end
        elseif lntype == 'N'
          name = lns{1}{startline+i}(12:end);
          noMoreDigits = true;
        elseif lntype == 'O'
          decEx = str2double(sc{1}{1}(12:end));
          break;
        end
      end
    end
  end

end

