N_SCAN=32;

COR_init

il_nsamp = 580;
il_sig_len = 25380;

il_bac_addr = 0;
il_sig_addr = 16;
il_pp_addr = il_sig_addr + il_sig_len;

nocal = 1;

for vc=1:N_SCAN                                                                       % signal, ionline + plasmaline, by wyh
  COR_fraclp(il_sig_addr, vc, 's', il_nsamp-2*15, 16, 24, (1:31)*12, 1)               % ionline signal lags, by wyh 
  COR_pp(il_pp_addr, 1, vc, 's', 1, il_nsamp, 0, 1)                                   % true power profile, by wyh 
end

for vc=1:N_SCAN                                                 % background, by wyh
  COR_pp(il_bac_addr, 1, vc, 'b', 1, 16, 0, 1)                                     % ionline background, by wyh
end

COR_end
