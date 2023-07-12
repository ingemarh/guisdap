COR_init

for vc = [2 4 6]	%% Power profile virtual channels
%for vc = [1 2 3]	%% Power profile virtual channels
	COR_pp( 750,1,vc,'s', 2,83,0,1)
	COR_pp( 833,1,vc,'b',29, 9,0,1)
	COR_pp( 842,1,vc,'c',29, 2,0,1)
end

for vc = [1 3 5]	%% Power profile virtual channels
	COR_pp(1594,1,vc,'s', 2,83,0,3)
	COR_pp(1677,1,vc,'b',29, 9,0,3)
	COR_pp(1686,1,vc,'c',29, 2,0,3)
end

for vc = [8 10 12]	%% Long pulse virtual channels
%for vc = [4 5 6]	%% Long pulse virtual channels
	COR_lp(   0,30,vc,'s',29,0,20,(0:29)*15,2)
	COR_lp( 600,30,vc,'b',29,0, 3,(0:29)*15,2)
	COR_lp( 690,30,vc,'c',29,0, 2,(0:29)*15,2)
end

for vc = [7 9 11]	%% Long pulse virtual channels
	COR_lp( 844,30,vc,'s',29,0,20,(0:29)*15,4);
	COR_lp(1444,30,vc,'b',29,0, 3,(0:29)*15,4);
	COR_lp(1534,30,vc,'c',29,0, 2,(0:29)*15,4);
end

COR_end
