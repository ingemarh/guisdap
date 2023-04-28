%construct proper power profiles
addr=1:120;
d_data(addr)=real(d_data(addr))+imag(d_data(addr));
%remove imaginary part of zerolag
addr=121+(0:16)*16;
d_data(addr)=real(d_data(addr));
%join pp channels
d_data(1:30)=sum(reshape(d_data(1:120),30,4),2);

