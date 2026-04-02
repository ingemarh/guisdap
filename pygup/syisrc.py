#!/bin/env python3
from ctypes import *
import numpy as np
import time
import datetime
import os
import sys

class syisr_header(Structure):
    _fields_ = [
        ('begflag'           , c_uint32   ),
        ('Pad0'              , c_uint16*4 ),
        ('month'             , c_uint8    ),
        ('year'              , c_uint8    ),
        ('hour'              , c_uint8    ),
        ('day'               , c_uint8    ),
        ('second'            , c_uint8    ),
        ('minute'            , c_uint8    ),
        ('fracOfSecond'      , c_uint16   ),
        ('Pad1'              , c_uint16*(16-10)   ),
        ('RadarFre'          , c_uint8    ),
        ('Pad2'              , c_uint8    ),
        ('Pad3'              , c_uint16*(18-17)   ),
        #('SampleRateCode'    , c_ubit4    ),
        #('WaveTypeCode'      , c_ubit4    ),
        #('Pad4'              , c_ubit4    ),
        #('BandWidthC'        , c_ubit4    ),
        ('SampleRate_WaveType', c_uint8    ),
        ('Pad4_BandWidthC'   , c_uint8    ),
        ('Pad5'              , c_uint16   ),
        ('ModeOfDetect'      , c_uint16   ),
        ('Pad6'              , c_uint16*(334-21)  ),
        ('CodeWidthV'        , c_uint16   ),
        ('Pad7'              , c_uint16*(382-335) ),
        ('DBFNumberOfOutput' , c_uint8    ),
        ('Pad8'              , c_uint8    ),
        ('WaveGateFrontV'    , c_uint16   ),
        ('WaveGateWidthV'    , c_uint32   ),
        ('Pad9'              , c_uint16*(428-386) ),
        ('Azi'               , c_uint16   ),
        ('Ele'               , c_uint16   ),
        ('Pad10'             , c_uint16*(434-430) ),
        ('AziT'              , c_uint16   ),
        ('EleT'              , c_uint16   ),
        ('GatefroT'          , c_uint16   ),
        ('GatetoT'           , c_uint16   ),
        ('Pad11'             , c_uint16*(468-438) ),
        ('BigCode'           , c_uint16   ),
        ('Pad12'             , c_uint16*2 ),
        ('LittleCode'        , c_uint16   ),
        ('Pad13'             , c_uint16*(490-472) ),
        ('PRTV'              , c_uint16   ),
        ('Pad14'             , c_uint16*3 ),
        ('PulseWidthV'       , c_uint16   ),
        ('Pad15'             , c_uint16*(500-495) ),
        ('syntTimeCode'      , c_uint16*2 ),
        ('Pad16'             , c_uint16*8 ),
        ('endflag'           , c_uint32   ),
]

class syisr_codes(Structure):
    _fields_ = [
        ('DetectMode'   , c_wchar_p ),
        ('BandWidth'    , c_float ),
        ('SampleRate'   , c_float ),
        ('WaveType'     , c_wchar_p ),
        ('RadarFreq'    , c_float ),
        ('SyntTime'     , c_float ),
]

class hdfhead(Structure):
    _fields_ = [
        ('dt' , c_uint64 ),
        #('bc' , c_uint16 ),
        #('lc' , c_uint16 ),
        #('az' , c_float ),
        #('el' , c_float ),
        #('pw' , c_uint16 ),
        #('bw' , c_uint16 ),
        ('ipp', c_uint16 ),
        ('ws' , c_uint16 ),
        ('at' , c_float ),
        ('et' , c_float ),
        ('rf' , c_float ),
        ('ex' , c_wchar_p ),
        ('st' , c_wchar_p ),
]

class hdfdata(Structure):
    _fields_ = [
        ('i' , c_int16 ),
        ('r' , c_int16 ),
]

def searchhead(f,fsize,d):
    FrameBegFlagUint=0xAAAA5555;
    while d!=FrameBegFlagUint:
        #print('%x'%d)
        d=np.fromfile(f,dtype='uint32',count=1)[0]
    f.seek(-sizeof(c_uint32),1)
    ftell=f.tell()
    return gethead(f),ftell

def gethead(f):
    if type(f)==str: f=open(f,'rb')
    FrameBegFlagUint=0xAAAA5555;
    FrameEndFlagUint=0xAA5555AA;
    head=syisr_header()
    if f.readinto(head)!=sizeof(head):
        return None
    elif head.begflag!=FrameBegFlagUint:
        return head.begflag
    elif head.endflag!=FrameEndFlagUint:
        return head.begflag
    #print('%x %x'%(head.begflag,head.endflag))
    return head

def getheadm(f):
    s=gethead(f)
    return dict((field, getattr(s,field)) for field,_ in s._fields_)

def getIQ(f,TotalIQ):
    if type(f)==str: f=open(f,'rb')
    IQ=np.fromfile(f,dtype='int32',count=TotalIQ)
    return IQ

def flist(f,extra_samples=0):
    if type(f)==str: f=open(f,'rb')
    tid=[]
    code=[]
    az=[]
    el=[]
    hdx=[]
    nd=[]
    nex=extra_samples
    ftell=f.tell()
    fsize=f.seek(0,os.SEEK_END)
    f.seek(ftell)
    while ftell<fsize:
        fpos=ftell
        h=gethead(f)
        if h==None:
          return tid,code,az,el,hdx,nd
        while type(h)==int:
          f.seek(-sizeof(syisr_header)-2*sizeof(c_int32),1)
          h,ftell=searchhead(f,fsize,h)
          if extra_samples==0:
              nex=(ftell-fpos)//(2*sizeof(c_int32))
              print('Found %d extra IQ sample, adjusting size'%nex)
              nd[len(nd)-1]+=nex
        #print(h.month,h.year,h.endflag)
        TotalIQ=(h.WaveGateWidthV+nex)*2
        #print(TotalIQ)
        if h.WaveGateWidthV>=0:
            btime=datetime.datetime(h.year+2000,h.month,h.day,h.hour,h.minute,h.second,h.fracOfSecond*25)
            tid.append(btime.timestamp()-8*3600)
            code.append(h.BigCode+h.LittleCode)
            az.append(h.Azi*0.005493164)
            el.append(h.Ele*0.005493164)
            hdx.append(ftell)
            nd.append(h.WaveGateWidthV+nex)
            f.seek(TotalIQ*sizeof(c_int32),1)
        else:
            print(h.WaveGateWidthV,h.WaveGateFrontV,h.PRTV)
        ftell=f.tell()
    return tid,code,az,el,hdx,nd

def struct2dict(s):
    return dict((field, getattr(s,field)) for field,_ in s._fields_)

def exp(head):
    CodeWidth=head.CodeWidthV+1
    if head.PulseWidthV==CodeWidth:
        exp='sy%d'%head.PulseWidthV
    else:
        exp='sy%dx%d'%(head.PulseWidthV/CodeWidth,CodeWidth)
    return exp

def gethdf(f,hdx,nd):
    if type(f)==str: f=open(f,'rb')
    f.seek(hdx,0)
    h=gethead(f)
    hh=hdfhead()
    btime=datetime.datetime(h.year+2000,h.month,h.day,h.hour,h.minute,h.second,h.fracOfSecond*25)
    hh.dt=int(1e6*(btime.timestamp()-8*3600))
    hh.ex=exp(h)
    hh.ipp=h.PRTV
    hh.ws=h.WaveGateFrontV
    hh.at=h.AziT*0.005493164
    hh.et=h.EleT*0.005493164
    hh.rf=h.RadarFre*0.2+430e6
    hh.st=os.path.basename(f.name)[0]
    if hh.st=='2': hh.st='S'
    #TotalIQ=h.WaveGateWidthV*2
    TotalIQ=nd*2
    s2=1.414213562
    #IQ32//=np.array(2,dtype='i4')
    IQ32=(getIQ(f,TotalIQ)/np.array(s2)).round()
    IQ=IQ32.astype(c_int16)
    nflow=0
    nmflow=-1
    for i in range(TotalIQ):
        if IQ32[i]>32767:
            IQ[i]=32767
            nmflow=i
            nflow+=1
            mIQ=IQ32[i]
        elif IQ32[i]<-32768:
            IQ[i]=-32768
            nmflow=i
            nflow+=1
            mIQ=-IQ32[i]
    #IQ=getIQ(f,TotalIQ).astype(c_int16)
    if nmflow>nflow*s2 and mIQ/s2>32767:
        print('Large overflow:',nflow,nmflow,mIQ)
    return hh,IQ

def printstr(s):
    for field, field_type in s._fields_:
        if field[:3]!='Pad':
            print(f"{field}: {getattr(s,field)}")

def getcodes(h):
    SampleRateC=[4.,0.1,0.2,0.4,20.,16.,0.]
    WaveTypeC=['LinFreqMod','CC','Barker','AC','LP','?','?','SingleCarrier','']
    BandWidthC=[0.05,4,0.1,0.3,1.,20.,16.,0.]
    DetectModeC=['Zenith','S-N Scan','W-E Scan','All Sky Scan','Other']
    c=syisr_codes()
    c.DetectMode=DetectModeC[min(h.ModeOfDetect,4)]
    c.BandWidth=BandWidthC[min(h.Pad4_BandWidthC//16,6)]
    c.SampleRate=SampleRateC[min(h.SampleRate_WaveType&15,6)]
    c.WaveType=WaveTypeC[min(h.SampleRate_WaveType//16,8)]
    c.RadarFreq=h.RadarFre*0.2+430
    c.SyntTime=h.syntTimeCode[0]*65536+h.syntTimeCode[1]+5
    return c

def getheadmat(f):
    s=gethead(f)
    h=dict((field, getattr(s,field)) for field,_ in s._fields_)
    s=getcodes(s)
    c=dict((field, getattr(s,field)) for field,_ in s._fields_)
    return h,c

def gethdfmat(f,i,n):
    s,iq=gethdf(f,i,n)
    h=dict((field, getattr(s,field)) for field,_ in s._fields_)
    return h,iq

def guess(f):
    if type(f)==str: f=open(f,'rb')
    h=gethead(f)
    btime=datetime.datetime(h.year+2000,h.month,h.day,h.hour,h.minute,h.second,h.fracOfSecond*25)
    dt=btime.timestamp()-8*3600
    site=os.path.basename(f.name)[0]
    if site=='2': site='S'
    s=getcodes(h)
    return dict(unx=dt,exp=exp(h),site=site,mode=s.DetectMode)

def filelist(dir,site='S',tsky=False,savedir=None):
    import re,scipy
    filen=[]
    l=np.empty((0,7))
    pattern=r'^%s._\d{4}_\d{2}_\d{2}_\d{2}_\d{2}_\d{2}\.dat\d{1}-\d{1}-\d+$'%site
    pattern_sy1=r'^\d{4}_\d{2}_\d{2}_\d{2}_\d{2}_\d{2}\.dat\d{1}-\d{1}-\d+$'
    for f in os.listdir(dir):
        if re.match(pattern,f) or re.match(pattern_sy1,f):
            tid,code,az,el,hdx,nd=flist(os.path.join(dir,f))
            filen+=[f]
            fno=[len(filen)]*len(tid)
            fl=np.c_[fno,tid,code,az,el,hdx,nd]
            l=np.concatenate((l,fl),axis=0)
        #else: print(f)
    if tsky:
        sp=sys.path[0]
        sys.path.append(sp)
        import add_tsky_main
        tid=l[:,1]
        az=l[:,3]
        el=l[:,4]
        t408=add_tsky_main.matface(os.path.join(sp,'..','matfiles'),site,tid,az,el)
        l=np.c_[l,t408]
    #print(l.shape)
    
    fname=dir.replace(os.sep,'__')
    if savedir==None:
        if os.access(dir,os.W_OK):
            savedir=dir
            fname='filelist'+site+'.mat'
        else:
            savedir=os.path.join(os.path.expanduser('~'),'gup','mydata','filelist')
            fname=dir.replace(os.sep,'__')+site+'.mat'
            if not os.path.isdir(savedir): os.makedirs(savedir)
    outf=os.path.join(savedir,fname)
    listf=l[l[:,1].argsort()]
    #print(list[0,:],lsort[0,:])
    scipy.io.savemat(outf,{'folder':dir,'fname':filen,'flist':listf},do_compression=True)
    return outf

def main():
    if len(sys.argv)<2:
        print("Usage: python script.py <binary file>")
        filename='/media/mop/datamop/syisr/bin/20250822/SY_2025_08_19_22_05_11.dat1-1-134'
        #sys.exit(1)
    else:
        filename=sys.argv[1]
    if os.path.isdir(filename):
        site='S'
        t408=False
        if len(sys.argv)>2: site=sys.argv[2]
        if len(sys.argv)>3: t408=sys.argv[3]
        filelist(filename,site,t408)
        exit(0)
    f=open(filename,'rb')

    tid,code,az,el,hdx,nd=flist(f)
    print(len(tid),type(tid))
    f.seek(hdx[0],0)
    head=gethead(f)
    printstr(head)
    c=getcodes(head)
    printstr(c)
    f.seek(0,0)
    print(guess(f))
    hh,IQ=gethdf(f,hdx[0],nd[0])
    printstr(hh)
    print(struct2dict(hh))
    print(len(IQ)/2)
    #print(IQ)
    #DataIQ = complex(signalIQ(2:2:end), signalIQ(1:2:end));
    #for hi in hdx:
    #    gethdf(f,hi)

if __name__ == "__main__":
    main()
