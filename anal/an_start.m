% an_start: Main program for the data analysis
% GUISDAP v.8.2 03-08-27 Copyright EISCAT, Huuskonen&Lehtinen
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

globals          % Defines (nearly) all global variables 
chk_par1
nat_const
get_ADDRSHIFT
init_graphics
spektri_init % Loads in plasma dispersion function table
name_antennas={'32m' '42m' 'vhf' 'uhf' 'kir' 'sod'};
radar_freqs=[500 500 224 930 930 930]*1e6;
radar_gains=10.^[4.25 4.48 4.31 4.81 4.81 4.81];
radar_effs=[.66 .68 .64 .66 .66 .66];

old_rcprog=-1; old_point=[-1 -1];
EOF=0;
while ~EOF
 
  if any(a_simul)
    OK=1;EOF=1;
  else
    if a_rawdata
      [OK,EOF]=integr_NW;
    else
      [OK,EOF,N_averaged,M_averaged]=integr_data;
    end
    if OK
      if length(d_parbl)==128
        decodeparblock_nd
      else
        decodeparblock
      end
      fprintf('\n%s-%s integrated\n',datestr(datenum(d_time(1,:)),0),...
        datestr(datenum(d_time(2,:)),13))
    end
  end
  d_date=datenum(d_time(1,:));

%****************************************************************************
% At this point, an integrated complex data dump is stored in variable d_data
%****************************************************************************

  if OK & a_do
    if d_rcprog~=old_rcprog | ((name_site=='K' | name_site=='S') & any(fix((old_point-[ch_el(1) ch_az(1)])/.05)))
      name_ant=char(name_antennas(ant_id));
      load_initfile
      read_antpar=[radar_freqs(ant_id) radar_gains(ant_id)];
      if any(abs(1-read_antpar./[ch_fradar(1) ch_gain(1)])>.1)
        ch_fradar=read_antpar(1)*ones(size(ch_fradar));
        ch_gain=read_antpar(2)*ones(size(ch_gain));
        warning('GUISDAP:default','Changed radar freq and antenna gain from init')
      end
      Ant_eff=radar_effs(ant_id);
      if a_control(4)>=2, load_GUPvar, end  
      if any(a_simul), simulparblock; end
      if exist([path_expr 'guizard.m'])==2
        run([path_expr 'guizard'])
      end
      GUIZARD
      scale_lpgwom % scales the spectral ambiguity function with lpg_ND factors
      form_adpar
      % Removing uncessary variables
      clear lpg_wom vc_Aenv vc_Apenv vc_penvabs ad_coeff_no_Pt
    end
    ch_Pt=ch_Pt(1)*ones(size(ch_fradar));
    if exist('N_averaged')
      if a_control(4)==1 & M_averaged(2)<6
        var_prof(N_averaged,M_averaged,6)
      else
        d_var1=d_var1-d_data.*d_data./N_averaged;
        d_var2=d_var2-d_data.*conj(d_data)./N_averaged;
      end
      if diff(M_averaged) % satellites found and all data normalised
        d_data(find(~N_averaged))=NaN;
        d=find(N_averaged<M_averaged(1));
        d1=d(find(N_averaged(d)>0));
        N_averaged(d1)=M_averaged(1)./N_averaged(d1);
        d_data(d)=d_data(d).*N_averaged(d);
      	if a_control(4)==1 & M_averaged(2)>=6
          d_var1(d)=d_var1(d).*N_averaged(d).^2;
          d_var2(d)=d_var2(d).*N_averaged(d).^2;
      	end
      end
    end
    if exist([path_expr 'guispert.m'])==2
      run([path_expr 'guispert'])
    end
    force2ch, GUISPERT
    constants

    if ~exist('ad_coeff_no_Pt','var')
      ad_coeff_no_Pt=radar_eq(Ant_eff); % calculates the radar constant      
    end
    ch_PtM=ch_Pt./a_Magic_const; %compensate for any inaccuracies
    for sig=find(lpg_bcs=='s')
      lp=lpg_lp(sig); addr=ADDR_SHIFT+lpg_addr(sig);
      vc=min(lp_vc(lp(1)),length(ch_PtM));
      ad_coeff(addr)=ad_coeff_no_Pt(addr)*ch_PtM(vc);
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
      indr=1:length(d_data); indi=indr;
      indi(find(imag(d_data)==0))=NaN; 
      drawnow, figure(di_figures(1)); %clf;
      plot(indr-1,real(d_data),'r',indi-1,imag(d_data),'b')
      indr=get(gca,'ylim'); indr=min([max([indr;-1000 -1000]);10000 10000]);
      set(gca,'ylim',indr)
      title(' Correlator dump'); 
      xlabel('Address'); ylabel('Power [K]'); grid; drawnow
      clear indr indi
    end
    if ~isempty(a_addr) & ch_Pt(1)>0
      get_apriori(any(a_simul))
%******************************************************************
% The get_apriori call calculated the raw electron density profile. 
% It is stored in variables
% pp_range   : range to power measurements
% pp_profile : Ne with a priori temperature ratio model
% pp_sigma   : Ne with Te=Ti
%******************************************************************
      clear_results
      if ~a_pponly
        half_prof
        if exist([path_expr 'guiditor.m'])==2
          run([path_expr 'guiditor'])
        end
        GUIDITOR
      end
      save_results
      if di_figures(4)
        drawnow, figure(abs(di_figures(4)))
        if di_figures(4)<0 & a_savespec | di_spectra
          plot_specs
        else
          plot_fit('panel',[1 1 1 0 1],[-inf inf 10*ceil((max(r_h)-min(r_h))/100)]);
        end
        drawnow 
      end
    end
  end 

end
if ~isempty(a_addr)
  if a_NCAR
    NCAR_output
    if a_realtime & isunix & ~isempty(local.site)
      do_NCAR([],2)
    else
      do_NCAR([],a_NCAR)
    end
  end
  if di_figures(5)
    vizu('new','rtgup')
    if di_figures(5)>1
      vizu('save',[],'print')
    else
      vizu('save')
    end
  end
  send_www
end
