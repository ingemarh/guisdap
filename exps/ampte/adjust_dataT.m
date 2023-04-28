%construct proper power profiles
addr=1+[624:905 1218:1403];
d_data(addr)=real(d_data(addr))+imag(d_data(addr));
%remove imaginary part of zerolag
addr=1+[0:8:623 906:4:1217 1404:16:1899];
d_data(addr)=real(d_data(addr));
%join cal channels
%d_data(778+(1:64))=sum(reshape(d_data(778+(1:64*2)),64,2),2);
%d_data(1340+(1:32))=sum(reshape(d_data(1340+(1:32*2)),32,2),2);
%d_data(1836+(1:16))=sum(reshape(d_data(1836+(1:16*4)),16,4),2);
