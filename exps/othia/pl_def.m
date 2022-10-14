if d_parbl(41)==8 %ESR

    ran=[47, 455; 253, 663];
    nfft=0; nint=1; ngates=2; nlag=1125;
    maxe=2; nup_d=2; skip_if=0;
    freq=[-3.6, -6, 3.6, 6]*1e6;
    dt=0.4e-6; invert=1; fradar=500e6;
    ele=81.6; updown=0:1;

    startad=(0:3)*ngates*nlag+1;

    if length(d_data)==9048 %If integrated without plasmaline selected
        startad=12+(0:3)*2262+1;
    end
end