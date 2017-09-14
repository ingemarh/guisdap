if exist('a_lagprofiling') && a_lagprofiling.do==1
 lpg_ND(3:32898)=lpg_ND(3:32898)/512*(diff(a_lagprofiling.p)+1);
end
