ch_Pt=ch_Pt(1);
d_data(7465:7472)=sum(reshape(d_data(7465:7496),8,4)')';
d_data(7473:7480)=sum(reshape(d_data(7497:7528),8,4)')';
if a_control(4)==1
 d_var1(7465:7480)=2*d_var1(7465:7480);
end
