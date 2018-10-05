classdef CODATA2006
%CODATA2006 Committee on Data for Science and Technology (CODATA) 2006 physical constants
% For more information, see the <a href="matlab: 
% web('http://physics.nist.gov/cuu/Constants/RevModPhys_80_000633acc.pdf')">NIST document</a>.
% Copyright(c) 2014 by Jonathan Kipling Knight (<a href="matlab:
% web('mailto:drkipknight@aol.com')">drkipknight@aol.com</a>)
% DEPENDENCY: this class depends on the OEIS class
% See also OEIS
  properties (Constant)
    % speed of light in a vacuum ( m/s )
    % c=1/sqrt(mu_0 epsilon_0)
    speedOfLightInVacuum = 299792458.0;
    % magnetic constant (permeability of vacuum) (N A^(-2))
    % mu_0=4 pi 10^(-7)
    magneticConstant = 4.0e-7*OEIS.Pi;
    % Newtonian constant of gravitation (m^3 kg^(-1) s^(-2))
    % G in force equation F=G M m r^(-2)
    newtonianGravitationalConstant = 6.67428e-11;
    % Planck constant ( J s )
    % h in momentum equation p=h/lambda
    planckConstant = 6.62606896e-34;
    % reduced Planck constant (J s)
    % hbar=h/(2 pi)
    reducedPlanckConstant = CODATA2006.planckConstant/(2.0*OEIS.Pi);
    % Molar gas constant ( J/(mol K) )
    % R in ideal gas equation P V = n R T
    molarGasConstant = 8.314472;
    % Avogadro constant ( 1/mol )
    % N_A
    avogadroConstant = 6.02214179e23;
    % elementary charge ( C )
    % e, charge of an electron in Coulombs
    elementaryCharge = 1.602176487e-19;
    % electron mass ( kg )
    % m_e, mass of an electron in kilograms
    electronMass = 9.10938215e-31;
    % proton (H-1 nucleus) mass ( kg )
    % m_p, mass of an proton in kilograms
    protonMass = 1.672621637e-27;
    % muon - mass ( kg )
    % m_mu, mass of an muon in kilograms
    muonMass = 1.88353130e-28;
    % tau - mass ( kg )
    % m_tau, mass of an tau in kilograms
    tauMass = 3.16777e-27;
    % neutron mass ( kg )
    % m_n, mass of an neutron in kilograms
    neutronMass = 1.674927211e-27;
    % deuteron (H-2 nucleus) mass ( kg )
    % m_d, mass of an deuteron in kilograms
    deuteronMass = 3.34358320e-27;
    % triton (H-3 nucleus) mass ( kg )
    % m_t, mass of an triton in kilograms
    tritonMass = 5.00735588e-27;
    % helion (He-3 nucleus) mass ( kg )
    % m_h, mass of an helion in kilograms
    helionMass = 5.00641192e-27;
    % alpha particle (He-4 nucleus) mass ( kg )
    % m_alpha, mass of an alpha particle in kilograms
    alphaParticleMass = 6.64465620e-27;
    % Conductance Quantum ( S )
    % G_0
    conductanceQuantum = 2*CODATA2006.elementaryCharge* ...
                            CODATA2006.elementaryCharge/ ...
                            CODATA2006.planckConstant;
    % Magnetic Flux Quantum ( Wb )
    % Phi_0
    magneticFluxQuantum = CODATA2006.planckConstant/(2* ...
                          CODATA2006.elementaryCharge);
    % fine structure constant ( unitless )
    % alpha
    fineStructureConstant = CODATA2006.magneticConstant* ...
                            CODATA2006.speedOfLightInVacuum* ...
                            CODATA2006.elementaryCharge* ...
                            CODATA2006.elementaryCharge/(2* ...
                            CODATA2006.planckConstant);
    % Rydberg constant (1/m)
    % R_infty
    rydbergConstant = CODATA2006.fineStructureConstant* ...
                      CODATA2006.fineStructureConstant* ...
                      CODATA2006.electronMass* ...
                      CODATA2006.speedOfLightInVacuum/(2* ...
                      CODATA2006.planckConstant);
    % Bohr radius (m)
    % a_0
    bohrRadius = CODATA2006.fineStructureConstant/(2*OEIS.Pi* ...
                 CODATA2006.rydbergConstant);
    % Josephson constant ( Hz/V )
    josephsonConstant = 2*CODATA2006.elementaryCharge/ ...
                          CODATA2006.planckConstant;
    % conventional Josephson constant ( Hz/V )
    conventionalJosephsonConstant = 483597.9e9;
    % von Klitzing constant ( Ohm )
    vonKlitzingConstant = CODATA2006.planckConstant/ ...
                         (CODATA2006.elementaryCharge* ...
                          CODATA2006.elementaryCharge);
    % conventional von Klitzing constant ( Ohm )
    conventionalVonKlitzingConstant = 25812.807;
    % standard atmospheric pressure at sea level ( Pa )
    standardAtmosphere = 101325.0;
    % Boltzmann constant ( J/K )
    % k=R/N_A
    boltzmannConstant = CODATA2006.molarGasConstant/ ...
                        CODATA2006.avogadroConstant;
    % electric constant (F/m)
    % permittivity of vacuum epsilon_0=1/(mu_0 c^2)
    electricConstant = 1.0/(CODATA2006.magneticConstant* ...
                            CODATA2006.speedOfLightInVacuum* ...
                            CODATA2006.speedOfLightInVacuum);
    % characteristic impedance of vacuum (Ohm)
    % Z_0=sqrt(mu_0/epsilon_0)=mu_0 c
    characteristicImpedance = CODATA2006.magneticConstant* ...
                              CODATA2006.speedOfLightInVacuum;
    % atomic mass constant (kg)
    % m_u=(1/12)mass of carbon 12=1 u=10^(-3)kg mol^(-1)/N_A
    atomicMassConstant = 1.0e-3/CODATA2006.avogadroConstant;
    % Planck mass ( kg )
    % m_P=sqrt(hbar c/G)
    planckMass = sqrt(CODATA2006.reducedPlanckConstant* ...
                      CODATA2006.speedOfLightInVacuum/ ...
                      CODATA2006.newtonianGravitationalConstant);
    % Planck energy ( J )
    % E_P=m_P c^2
    planckEnergy = CODATA2006.planckMass* ...
                   CODATA2006.speedOfLightInVacuum* ...
                   CODATA2006.speedOfLightInVacuum;
    % Planck temperature ( K )
    % T_P=m_P c^2/k
    planckTemperature = CODATA2006.planckEnergy/ ...
                        CODATA2006.boltzmannConstant;
    % Planck length ( m )
    % l_P=hbar/(m_P c)
    planckLength = CODATA2006.reducedPlanckConstant/ ...
                  (CODATA2006.planckMass* ...
                   CODATA2006.speedOfLightInVacuum);
    % Planck time ( s )
    % t_P=l_P/c
    planckTime = CODATA2006.planckLength/ ...
                 CODATA2006.speedOfLightInVacuum;
    % first radiation constant ( W m^2 )
    % c_1=2 pi h c^2
    firstRadiationConstant = CODATA2006.planckConstant* ...
                             CODATA2006.speedOfLightInVacuum* ...
                             CODATA2006.speedOfLightInVacuum* ...
                             2.0*OEIS.Pi;
    % first radiation constant ( W m^2 / sr )
    % c_1L=2 h c^2
    firstRadiationSpectralRadianceConstant = CODATA2006.planckConstant* ...
                             CODATA2006.speedOfLightInVacuum* ...
                             CODATA2006.speedOfLightInVacuum* ...
                             2.0;
    % second radiation constant ( m K )
    % c_2=h c/k
    secondRadiationConstant = CODATA2006.planckConstant* ...
                              CODATA2006.speedOfLightInVacuum/ ...
                              CODATA2006.boltzmannConstant;
    % Stefan-Boltzmann constant ( W m^(-2) K^(-4) )
    % sigma=(pi^2/60)k^4/(hbar^3 c^2)
    % sigma in total luminosity equation L= sigma T^4
    stefanBoltzmannConstant = CODATA2006.firstRadiationConstant/ ...
                             (CODATA2006.secondRadiationConstant^4)*6.0* ...
                              OEIS.A013662;
    % Wien displacement law constant for maximum wavelength (m K)
    % b=c_2/alpha_5=lambda_max T
    % solve transcendental equation alpha_5/(1-e^(-alpha_5))=5
    % alpha_5=5+W(-5/e^5)=4.965114231744276 where W(x) is the Lambert W-Function.
    wienDisplacementWavelengthLawConstant = CODATA2006.secondRadiationConstant/ ...
                                            OEIS.A094090;
    % Wien displacement law constant for maximum frequency (Hz/K)
    % b'=alpha_3 c/c_2=nu_max/T
    % solve transcendental equation alpha_3/{1-e^(-alpha_3)}=3
    % alpha_3=3+W(-3/e^3)=2.821439372
    % alpha_3 Solution from Mathematica:
    % N[3+ProductLog[-3/Exp[3]],40]
    wienDisplacementFrequencyLawConstant = 2.821439372122078893403191330294485195346* ...
                                           CODATA2006.speedOfLightInVacuum/ ...
                                           CODATA2006.secondRadiationConstant;
    % Sackur-Tetrode absolute entropy at 100000 Pa (unitless)
    sackurTetrodeConstant1 = CODATA2006.sackurTetrodeConstant(1.0,1.0e5);
    % Sackur-Tetrode absolute entropy at 101325 Pa (unitless)
    sackurTetrodeConstant2 = CODATA2006.sackurTetrodeConstant(1.0,CODATA2006.standardAtmosphere);
  end
  methods (Static)
    function entropy = sackurTetrodeConstant(T1,p0)
    % sackurTetrodeConstant Sackur-Tetrode absolute entropy (unitless)
    % INPUT:
    %   T1 - temperature (K)
    %   p0 - pressure (Pa)
      entropy = (5.0/2)+log(((2*OEIS.Pi*CODATA2006.atomicMassConstant* ...
                                        CODATA2006.boltzmannConstant*T1/ ...
                                        CODATA2006.planckConstant^2)^1.5)* ...
                                        CODATA2006.boltzmannConstant*T1/p0);
    end
  end

end

