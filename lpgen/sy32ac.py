#!/usr/bin/env python3
def myexp(site):
	try:
		import lpgen.par_gen as par_gen
	except:
		import par_gen
	###### Setup section
	dspexp='sy32ac'		#Experiment version
	version=1		#Experiment version
	cal_samp=0		#Number of calibration and bakground samples
	loops=64		#Number of loops in a complete cycle
	code_len=32		#Number of Bauds in each code
	nr_codes=64		#Number of codes
	code_tx=32		#Number of Bauds to send
	nr_loop=70		#Number of loops to get wanted integration time 
	plasma_pulses=0		#Number of pulses to correlate
	plasma_frac=5		#Plasma frac
	ndgat=0
	dlong=0
	dlong=0
	nr_pulses=1		#Number of pulses to correlate
	ion_frac=2
	ion_lag=64		#Maxlag for the ion line
	mthread=20
	dshort=0
	ipp=15000	#IPP length in us
	baud_len=20	#Baud length in us
	start_tx=0	#When to start transmitting in us
	trx_frq=430	#Transmit frequency used
	start_samp=int(100.05/0.15)	#When to start sampling
	isamp=240
	if site=='r':
		#ion_lag=63
		lowtail=0
		toptail=0
	else:
		site='3'
		lowtail=code_tx-2
		toptail=0
		#version=2 # 20250325
		nr_cal=isamp%ion_frac
		#plasma_pulses=1		#Number of pulses to correlate
		#plasma_frac=80		#Plasma frac
	if version==1:
		start_samp=int(99.981/0.15)	#When to start sampling
		isamp=1268/2
		nr_loop=1		#Number of loops to get wanted integration time 
		ipp=14000	#IPP length in us
		nr_cal=isamp
	calstop=ipp-1
	tails=lowtail+toptail
	samp_speed=baud_len/ion_frac
	start_samp+=start_tx	# When to start sampling in us
	print('Doing experiment files for site='+site)
	exp_name=dspexp+'_'+site#Name of experiment

	######
	nr_fullgates=isamp//ion_frac-(code_tx-1)
	if site=='r':
		clutts=0
	else:
		clutts=150	#Clutter window us
		if version==1:
			clutts=0	#Clutter window us
	print(isamp,clutts)
	ac_code=par_gen.acgen(code_len,code_tx,nr_codes,'b')
	if version!=1:
		dspexp=(dspexp+'_%d')%version
	exp_name=dspexp+'_'+site	#Name of experiment
	if site=='3':
		isamp=par_gen.plwingen(nr_pulses,plasma_pulses,plasma_frac,code_tx,nr_fullgates,dspexp,site,ion_frac,tails,mthread,nr_codes,nr_loop,dshort,dlong,ndgat,clutts,toptail,lowtail,loops,ac_code,nr_cal)+nr_cal
		print(isamp,clutts)
		par_gen.t2ps(cal_samp,samp_speed,loops,baud_len,ac_code,code_len,code_tx,start_tx,ipp,trx_frq,site,dspexp,start_samp,isamp,calstop)
	elif site=='r':
		isamp=par_gen.plwingen(nr_pulses,plasma_pulses,plasma_frac,code_tx,nr_fullgates,dspexp,site,ion_frac,tails,mthread,nr_codes,nr_loop,dshort,dlong,ndgat,clutts,toptail,lowtail,loops,ac_code,0)
		print(isamp,clutts)
		par_gen.cluttgen(exp_name,loops,nr_loop,isamp,clutts,ion_frac)
		par_gen.t2ps(cal_samp,samp_speed,loops,baud_len,ac_code,0,0,start_tx,ipp,trx_frq,site,dspexp,start_samp,isamp,calstop)
