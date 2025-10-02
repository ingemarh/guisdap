from ctypes import *
import numpy as np
import time
import datetime
import os

class syisr_header(Structure):
    _fields_ = [
        ('Pad0'              , c_uint16*6 ),
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

def gethead(f):
    if type(f)==str: f=open(f,'rb')
    FrameEndFlagUint=0xAA5555AA;
    head=syisr_header()
    f.readinto(head)==sizeof(head)
    endflag=head.endflag
    if endflag!=FrameEndFlagUint:
        exit('dont read to end flag');
    #print(head.month,head.year)
    return head

def getheadm(f):
    s=gethead(f)
    return dict((field, getattr(s,field)) for field,_ in s._fields_)

def getIQ(f,TotalIQ):
    if type(f)==str: f=open(f,'rb')
    IQ=np.fromfile(f,dtype='int32',count=TotalIQ)
    return IQ

def flist(f):
    if type(f)==str: f=open(f,'rb')
    tid=[]
    code=[]
    az=[]
    el=[]
    hdx=[]
    nd=[]
    ftell=f.tell()
    fsize=f.seek(0,os.SEEK_END)
    f.seek(ftell)
    while ftell<fsize:
        h=gethead(f)
        #print(h.month,h.year,h.endflag)
        TotalIQ = (h.WaveGateWidthV-0) * 2
        #print(TotalIQ)
        if h.WaveGateWidthV>=0:
            btime=datetime.datetime(h.year+2000,h.month,h.day,h.hour,h.minute,h.second,h.fracOfSecond*25)
            tid.append(btime.timestamp()-8*3600)
            code.append(h.BigCode+h.LittleCode)
            az.append(h.Azi*0.005493164)
            el.append(h.Ele*0.005493164)
            hdx.append(ftell)
            nd.append(h.WaveGateWidthV)
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
        exp='sy%dx'%head.PulseWidthV
    else:
        exp='sy%dx%d'%(head.PulseWidthV/CodeWidth,CodeWidth)
    return exp

def gethdf(f,hdx):
    if type(f)==str: f=open(f,'rb')
    f.seek(hdx,0)
    h=gethead(f)
    hh=hdfhead()
    btime=datetime.datetime(h.year+2000,h.month,h.day,h.hour,h.minute,h.second,h.fracOfSecond*25)
    hh.dt=int(1e6*(btime.timestamp()-8*3600))
    TotalIQ=h.WaveGateWidthV*2
    hh.ex=exp(h)
    hh.ipp=h.PRTV
    hh.ws=h.WaveGateFrontV
    hh.at=h.AziT*0.005493164
    hh.et=h.EleT*0.005493164
    hh.rf=h.RadarFre*0.2+430e6
    hh.st=os.path.basename(f.name)[0]
    if hh.st=='2': hh.st='S'
    TotalIQ=h.WaveGateWidthV*2
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
    c.DetectMode=DetectModeC[min(h.ModeOfDetect,5)]
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

def gethdfmat(f,i):
    s,iq=gethdf(f,i)
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

def main():
    import sys
    if len(sys.argv)!=2:
        print("Usage: python script.py <binary file>")
        filename='/media/mop/datamop/syisr/bin/20250822/SY_2025_08_19_22_05_11.dat1-1-134'
        #sys.exit(1)
    else:
        filename=sys.argv[1]
    f=open(filename,'rb')

    tid,code,az,el,hdx,nd=flist(f)
    print(len(tid),type(tid))
    f.seek(0,0)
    head=gethead(f)
    printstr(head)
    c=getcodes(head)
    printstr(c)
    f.seek(0,0)
    print(guess(f))
    hh,IQ=gethdf(f,hdx[0])
    printstr(hh)
    print(struct2dict(hh))
    print(len(IQ)/2)
    #print(IQ)
    #DataIQ = complex(signalIQ(2:2:end), signalIQ(1:2:end));
    #for hi in hdx:
    #    gethdf(f,hi)

if __name__ == "__main__":
    main()
