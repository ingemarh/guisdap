function [ JF ] = defaultIRIswitches( )
%DEFAULTIRISWITCHES set the default switches
  JF = zeros(IRI2012.numSwitches,1);
  JF(IRI2012.Ne_COMPUTED_SW) = 1; % Ne computed
  JF(IRI2012.TeTi_COMPUTED_SW) = 1; % Te, Ti computed
  JF(IRI2012.NeNi_COMPUTED_SW) = 1; % Ne & Ni computed
  %    4  B0,B1 - other models JF(31)
  %    5  foF2 - URSI
  %    6  Ni - RBV-2010 & TTS-2003
  JF(IRI2012.Ne_Tops_SW) = 1; %  Ne - Tops: f10.7<188
  JF(IRI2012.FOF2_MODEL_SW) = 1; % foF2 from model
  JF(IRI2012.HMF2_MODEL_SW) = 1; % hmF2 from model
  JF(IRI2012.Te_STANDARD_SW) = 1; % Te - Standard
  JF(IRI2012.Ne_STANDARD_SW) = 1; % Ne - Standard Profile
  JF(IRI2012.STANDARD_OUT_SW) = 1; % Messages to unit 6
  JF(IRI2012.FOF1_MODEL_SW) = 1; % foF1 from model
  JF(IRI2012.HMF1_MODEL_SW) = 1; % hmF1 from model
  JF(IRI2012.FOE_MODEL_SW) = 1; % foE  from model
  JF(IRI2012.HME_MODEL_SW) = 1; % hmE  from model
  JF(IRI2012.RZ12_FILE_SW) = 1; % Rz12 from file
  JF(IRI2012.IGRF_DIP_SW) = 1; % IGRF dip, magbr, modip old
  JF(IRI2012.F1PROB_MODEL_SW) = 1; % F1 probability model
  JF(IRI2012.F1_STANDARD_SW) = 1; % standard F1
  %   21    ion drift not computed
  JF(IRI2012.IONDENS_PERCENT_SW) = 1; % ion densities in %
  %   23  Te_topside (TBT-2012)
  JF(IRI2012.D_REGION_IRI1990_SW) = 1; % D-region: IRI-1990
  JF(IRI2012.F107D_FILE_SW) = 1; % F107D from APF107.DAT
  JF(IRI2012.FOF2_STORM_MODEL_SW) = 1; % foF2 storm model
  JF(IRI2012.IG12_FILE_SW) = 1; % IG12 from file
  %   28    spread-F probability not computed
  %   29    new options as def. by JF(30)   false
  %   30    NeQuick topside model   	     false 
  % (29,30) = (t,t) IRIold, (f,t) IRIcor, (f,f) NeQuick, (t,f) Gulyaeva
  JF(IRI2012.B0B1_ABT_2009_SW) = 1; % B0,B1 ABT-2009
  JF(IRI2012.F107_81_FILE_SW) = 1; % F10.7_81 from file
  %   33    Auroral boundary model on/off  true/false	             false
  JF(IRI2012.MESSAGES_ON_SW) = 1; % Messages on
  %   35    no foE storm updating           false
  JF(IRI2012.HMF2_WO_STORM_SW) = 1; % hmF2 w/out foF2_storm
  JF(IRI2012.TOPSIDE_WO_STORM_SW) = 1; % topside w/out foF2-storm
  JF(IRI2012.NO_WRITES_IRIFLIP_SW) = 1; % turn WRITEs off in IRIFLIP
%   ..    ....
%   50    ....

end

