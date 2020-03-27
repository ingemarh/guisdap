if exist('a_lpf') && a_lpf.do==1
 %lpg_ND(3:32898)=lpg_ND(3:32898)/512*(diff(a_lagprofiling.p)+1);
end
