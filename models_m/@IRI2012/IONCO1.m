function [ cn ] = IONCO1( h,zd,fd,fs,t )
%IONCO1 ion composition model
%
%        subroutine ionco1(h,zd,fd,fs,t,cn)
%---------------------------------------------------------------
% ion composition model
%   A.D. Danilov and A.P. Yaichnikov, A New Model of the Ion
%   Composition at 75 to 1000 km for IRI, Adv. Space Res. 5, #7,
%   75-79, 107-108, 1985
%
%       h       altitude in km
%       zd      solar zenith angle in degrees
%       fd      latitude in degrees (same result for fd and -fd)
%       fs      10.7cm solar radio flux
%       t       seasonal decimal month (Northern Hemisphere January 
%                   15 is t=1.5 and so is Southern Hemisphere July 15)
%       cn(1)   O+  relative density in percent
%       cn(2)   H+  relative density in percent
%       cn(3)   N+  relative density in percent
%       cn(4)   He+ relative density in percent
% Please note: molecular ions are now computed in IONCO2
%       [cn(5)   NO+ relative density in percent
%       [cn(6)   O2+ relative density in percent
%       [cn(7)   cluster ions  relative density in percent
%---------------------------------------------------------------

%        dimension       cn(7),cm(7),hm(7),alh(7),all(7),beth(7),
%     &                  betl(7),p(5,6,7),var(6),po(5,6),ph(5,6),
%     &                  pn(5,6),phe(5,6),pno(5,6),po2(5,6),pcl(5,6)
%        dimension       cn(4),cm(4),hm(4),alh(4),all(4),beth(4),
%     &                  betl(4),p(5,6,4),var(6),po(5,6),ph(5,6),
%     &                  pn(5,6),phe(5,6)
%
%        common  /argexp/argmax
%        common  /const/ umr
  persistent po ph pn phe NI NJ NK;
  if isempty(po)
    po = transpose([0.,0.,0.,0.,98.5; ...
                    0.,0.,0.,0.,320.; ...
                    0.,0.,0.,0.,-2.59E-4; ...
                    2.79E-4,-3.33E-3,-3.52E-3,-5.16E-3,-2.47E-2; ...
                    0.,0.,0.,0.,-2.5E-6; ...
                    1.04E-3,-1.79E-4,-4.29E-5,1.01E-5,-1.27E-3]);
    ph = transpose([-4.97E-7,-1.21E-1,-1.31E-1,0.,98.1; ...
                    355.,-191.,-127.,0.,2040.; ...
                    0.,0.,0.,0.,-4.79E-6; ...
                    -2.E-4,5.67E-4,2.6E-4,0.,-5.08E-3; ...
                    0.,0.,0.,0.,0.; ...
                    0.,0.,0.,0.,0.]);
    pn = transpose([7.6E-1,-5.62,-4.99,0.,5.79; ...
                    83.,-369.,-324.,0.,593.; ...
                    0.,0.,0.,0.,-6.3E-5; ...
                    -6.74E-3,-7.93E-3,-4.65E-3,0.,-3.26E-3; ...
                    0.,0.,0.,0.,-1.17E-5; ...
                    4.88E-3,-1.31E-3,-7.03E-4,0.,-2.38E-3]);
    phe = transpose([-8.95E-1,6.1,5.39,0.,8.01; ...
                     0.,0.,0.,0.,1200.; ...
                     0.,0.,0.,0.,-1.04E-5; ...
                     1.9E-3,9.53E-4,1.06E-3,0.,-3.44E-3; ...
                     0.,0.,0.,0.,0.; ...
                     0.,0.,0.,0.,0.]);
    %       data pno/-22.4,17.7,-13.4,-4.88,62.3,32.7,0.,19.8,2.07,115.,
    %    &          5*0.,3.94E-3,0.,2.48E-3,2.15E-4,6.67E-3,5*0.,
    %    &          -8.4E-3,0.,-3.64E-3,2.E-3,-2.59E-2/
    %       data po2/8.,-12.2,9.9,5.8,53.4,-25.2,0.,-28.5,-6.72,120.,
    %    &          5*0.,-1.4E-2,0.,-9.3E-3,3.3E-3,2.8E-2,5*0.,4.25E-3,
    %    &          0.,-6.04E-3,3.85E-3,-3.64E-2/
    %       data pcl/4*0.,100.,4*0.,75.,10*0.,4*0.,-9.04E-3,-7.28E-3,
    %    &          2*0.,3.46E-3,-2.11E-2/
    NI = 5;
    NJ = 6;
    NK = 4;
  end
  cn = zeros(7,1);
  cm = zeros(NK,1);
  hm = zeros(NK,1);
  alh = zeros(NK,1);
  all = zeros(NK,1);
  beth = zeros(NK,1);
  betl = zeros(NK,1);
  p = zeros(NI,NJ,NK);
  var = zeros(NJ,1);

  z=zd*IRI2012.UMR;
  f=fd*IRI2012.UMR;

  for i=1:NI
    for j=1:NJ
      p(i,j,1)=po(i,j);
      p(i,j,2)=ph(i,j);
      p(i,j,3)=pn(i,j);
      p(i,j,4)=phe(i,j);
      %               p(i,j,5)=pno(i,j);
      %               p(i,j,6)=po2(i,j);
      %               p(i,j,7)=pcl(i,j);
    end
  end

  s=0.;
  
  for i=1:NK
    for j=1:NJ
      var(j) = p(1,j,i)*cos(z) ...
             + p(2,j,i)*cos(f) ...
             + p(3,j,i)*cos(0.013*(300.-fs)) ...
             + p(4,j,i)*cos(0.52*(t-6.)) ...
             + p(5,j,i);
    end
    cm(i)  = var(1);
    hm(i)  = var(2);
    all(i) = var(3);
    betl(i)= var(4);
    alh(i) = var(5);
    beth(i)= var(6);
    hx=h-hm(i);
    if hx < 0 
      arg = hx * (hx * all(i) + betl(i));
      cn(i) = 0.;
      if arg > -IRI2012.ARGMAX
        cn(i) = cm(i) * exp( arg );
      end
    elseif hx == 0
      cn(i) = cm(i);
    else
      arg = hx * (hx * alh(i) + beth(i));
      cn(i) = 0.;
      if arg > -IRI2012.ARGMAX
        cn(i) = cm(i) * exp( arg );
      end
    end
    if cn(i) < 0.005*cm(i)
      cn(i)=0.;
    end
    if cn(i) > cm(i)
      cn(i)=cm(i);
    end
    s=s+cn(i);
  end
  
  if s ~= 0.0
    for i=1:NK
      cn(i)=cn(i)/s*100.;
    end
  end

end

