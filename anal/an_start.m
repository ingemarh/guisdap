% an_start: Main program for the data analysis
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% IH:ionomodel_control control
% Main program for the data analysis. The most important operations performed are
% routine name:  action:
% chk_par1       1) transforms the user supplied control parameters to internal parameters
%                2) checks the data source (matlab files/EISCAT .dtst files)
% init_graphics  opens a sufficient number of figure windows and defines there sizes etc.
% integr_NW      calls Nigel Wade's integration package, when EISCAT .dtst files are used
% integr_data    integrates data from Matlab files
% decodeparblock transfers the radar parameters (power etc) to internal GUISDAP parameters
% load_initfile  loads the ambiguity functions etc
% scale_lpgwom   scales the spectal amb. function with the correlator algorithm factors (lpg_ND)
% radar_eq       radar equation     
% scale_data     scales the data with the correlator algorithm factors (lpg_ND) 
% subr_backgr    background subtraction to the data  
% get_apriori    the a priori model, electron density obtained from the data
% half_prof      performs the gated analysis to the data 
% save_results   results stored to the disk
% plot_results   displays the results
%
% Other routines called: globals nat_const get_ADDRSHIFT  load_GUPvar GUIZARD GUISPERT form_adpar
%                       spektri_init constants chk_par2 simul_dump clear_results
  
t_init
t_start(1)

globals          % Defines (nearly) all global variables 
chk_par1
nat_const
get_ADDRSHIFT
init_graphics

old_rcprog=-1; old_point=[-1 -1];
EOF=0;
while ~EOF
 
  if any(a_simul)
    OK=1;EOF=1;
  else
    if a_rawdata,
      [OK,EOF]=integr_NW;
    else
      [OK,EOF,N_averaged]=integr_data;
    end
    if OK
      if length(d_parbl)==128
	decodeparblock_nd
      else
        decodeparblock
      end
    end
  end

%****************************************************************************
% At this point, an integrated complex data dump is stored in variable d_data
%****************************************************************************

  if OK
    if d_rcprog~=old_rcprog | ((name_site=='K' | name_site=='S') & any(old_point~=[ch_el(1) ch_az(1)]))
      name_ant={'32m','42m','vhf','uhf','kir','sod'};
      name_ant=char(name_ant(ant_id));
      load_initfile
      Ant_eff=.66;
      if name_site=='V'
        [ch_el ch_az ch_gain]=vhf_elaz(ch_el,ch_az,ch_gain);
        Ant_eff=.64;
      elseif name_site=='L' & ant_id==2
        ch_gain=(42/32)^2*ch_gain;
      end
      if a_control(4)>=2, load_GUPvar, end  
      if any(a_simul), simulparblock; end
      if exist('GUIZARD')==2, GUIZARD, end
      scale_lpgwom % scales the spectral ambiguity function with lpg_ND factors
      form_adpar
      spektri_init % Loads in plasma dispersion function table
      constants
      % Removing uncessary variables
      clear lpg_wom vc_Aenv vc_Apenv vc_penvabs vc_penv vc_penvo ad_coeff_no_Pt
    end
    if exist([path_expr 'guispert.m'])==2
      run([path_expr 'guispert'])
    end
    force2ch, GUISPERT

    if ~exist('ad_coeff_no_Pt','var')
      ad_coeff_no_Pt=radar_eq(Ant_eff); % calculates the radar constant      
    end
    for sig=find(lpg_bcs=='s')
      lp=lpg_lp(sig); addr=ADDR_SHIFT+lpg_addr(sig);
      vc=min(lp_vc(lp(1)),length(ch_Pt));
      ad_coeff(addr)=ad_coeff_no_Pt(addr)*ch_Pt(vc)/a_Magic_const;
    end
    ad_coeff=ad_coeff/a_Magic_const; % compensate for all the inaccuracies in the numeric constants

    if a_control(4)==1 & exist('N_averaged') & N_averaged<6
      var_prof(N_averaged,6)
    end
    chk_par2
    old_point=[ch_el(1) ch_az(1)];
    old_rcprog=d_rcprog;

    if any(a_simul)
      simul_dump
    else
      scale_data   
    end

%**************************************************************************
% The data has been scaled by calibration, so that it appears in units of K
% Now subtract the background
%**************************************************************************
    subr_backgr   

    if di_figures(1)
      figure(di_figures(1)); clf;
      indr=1:length(d_data); indi=indr;
      indi(find(imag(d_data)==0))=NaN; 
      figure(di_figures(1)); clf;
      plot(indr-1,real(d_data),'r',indi-1,imag(d_data),'b')
      indr=get(gca,'ylim'); indr=min([max([indr;-1000 -1000]);10000 10000]);
      set(gca,'ylim',indr)
      title(' Correlator dump'); 
      xlabel('Address'); ylabel('Power [K]'); grid; drawnow
      clear indr indi
    end
    get_apriori(any(a_simul))
%*******************************************************************************
% The get_apriori call calculated the raw electron density profile. 
% It is stored in variables
% pp_range   : range to power measurements
% pp_profile : Ne with a priori temperature ratio model
% pp_sigma   : Ne with Te=Ti
%*******************************************************************************
    if exist('ionomodel_control') & ionomodel_control==3
      ionomodel_control=0;
    end

    clear_results
%**************************************** 				
    half_prof 
%**************************************** 
    save_results
    if di_figures(4)
      figure(di_figures(4))
      plot_fit('panel',[1 1 1 0 1],[-inf inf 10*ceil((max(r_h)-min(r_h))/100)]);
      drawnow 
    end
  end 

end
t_stop(1)
t_result
if a_NCAR
  NCAR_output
end
if di_figures(5)
  vizu('new','rtgup')
  vizu('save')
end
send_www
