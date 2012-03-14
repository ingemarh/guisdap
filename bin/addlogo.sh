#!/bin/sh

pos1=780
rot1=
landscape=`grep landscapeMode $1/$2.$3 | grep csm`
echo $landscape
if [ "$landscape" ]
then
	pos1=100
	rot1="90 rotate"
	ed $1/$2.$3 <<!
/%%BeginObject
/landscapeMode
a
0 600 translate
.
w
q
!
fi

editor=ed
if ! `hash $editor 2>/dev/null`
then
	editor="vi -e"
fi
$editor $1/$2.$3 <<!
/%%Title
c
%%Title: $2
.
/%%BeginProlog
a
%%Title: EISCAT Logo
%%Creator: TvE/JM/CLH
%%CreationDate: 29 April 92
%%BoundingBox: -30 -30 30 30
/reencsmalldict 12 dict def /^reencodesmall
{reencsmalldict begin /newcodesandnames exch def
/newfontname exch def /basefontname exch def
/basefontdict basefontname findfont def
/newfont basefontdict maxlength dict def
basefontdict {exch dup /FID ne {dup /Encoding eq
{exch dup length array copy newfont 3 1 roll put}
{exch newfont 3 1 roll put} ifelse } {pop pop}
ifelse } forall newfont /FontName newfontname put
newcodesandnames aload pop newcodesandnames length 2 idiv
{newfont /Encoding get 3 1 roll put} repeat
newfontname newfont definefont pop end }def
/scandvec[ 8#300 /Udieresis 8#311 /Adieresis
8#321 /udieresis 8#331 /adieresis 8#333 /Odieresis
8#334 /odieresis 8#342 /Aring 8#362 /aring ]def
/outsidecircletext{ circtextdict begin /radius exch def
/centerangle exch def /ptsize exch def /str exch def
/xradius radius ptsize 4 div add def gsave
centerangle str findhalfangle add rotate str
{ /charcode exch def ( ) dup 0 charcode put outsideplacechar
} forall grestore end }def
/circtextdict 16 dict def circtextdict begin
/findhalfangle { stringwidth pop 2 div
2 xradius mul pi mul div 360 mul }def
/outsideplacechar { /char exch def
/halfangle char findhalfangle def gsave
halfangle neg rotate radius 0 translate -90 rotate
char stringwidth pop 2 div neg 0 moveto char show
grestore halfangle 2 mul neg rotate }def /pi 3.1415923 def end
/^eiscatlogo{ clear .15  setlinewidth
0 0 10 0 360 arc stroke
newpath 0   0 7.3 180.00   0.00 arcn stroke
newpath 0   0 7.5 234.30 305.70 arc  stroke
newpath 0 -14 9.0  44.81 135.5 arc  stroke
0.80 1 scale
newpath 0 -11 6 20.0 160.00 arc 1.25 1 scale stroke
0.40 1 scale
newpath 0 -11 6 12.5 167.50 arc 2.50 1 scale stroke
7.37 0 moveto 1.3 0 3.8 1.0 0.2 arcto stroke moveto
clear 5.4 1.75 0 1.75 0.2 arcto stroke moveto clear
-5.4 1.75 -3.8 1 0.2      arcto stroke moveto clear
-1.3 0 -7.3 0  0.2        arcto stroke moveto clear
-7.37 0 lineto stroke
0.00 1.75 moveto 0    7.30 lineto stroke
1.15 1.75 moveto 0.62 7.25 lineto stroke
2.30 1.75 moveto 1.20 7.18 lineto stroke
3.45 1.75 moveto 1.80 7.05 lineto stroke
4.59 1.68 moveto 2.35 6.91 lineto stroke
7.32 0.0 moveto 2.89 6.70 lineto stroke
-1.15 1.75 moveto -0.62 7.25 lineto stroke
-2.30 1.75 moveto -1.20 7.18 lineto stroke
-3.45 1.75 moveto -1.80 7.05 lineto stroke
-4.66 1.68 moveto -2.35 6.91 lineto stroke
-7.32 0.0 moveto -2.89 6.70 lineto stroke
2. 28.3464 div setlinewidth
0.0 1.73 moveto 5.0 -8.0 lineto -0.0 1.73 lineto
-5.0 -8.0 lineto closepath 0 -9.5 lineto -0.0 1.73 lineto stroke
2.0 1.73 moveto 0 -9.3 lineto -2.0 1.73 lineto stroke
-6.0 0.0 moveto 0 -6.8 lineto -6.0 0.0 lineto stroke
6.0 0.0  moveto  0 -6.8 lineto  6.0 0.0 lineto stroke
newpath -5.0 -8.0 0.2 0 360 arc fill stroke
newpath  0.0 -9.3 0.2 0 360 arc fill stroke
newpath  5.0 -8.0 0.2 0 360 arc fill stroke
newpath  0.0 -6.8 0.2 0 360 arc fill stroke
/Helvetica findfont 2.2 scalefont setfont
(EISCAT SCIENTIFIC ASSOCIATION) 2.4 90 7.8
outsidecircletext stroke }def
/Helvetica /Helvetica-S scandvec ^reencodesmall
/^engaddress{
/Helvetica-Bold findfont 10 scalefont setfont
0 5 moveto (EISCAT Scientific Association)show
/Helvetica findfont 3 scalefont setfont
56 -2 moveto
(   )
show
55 -6 moveto
(   )
show
}def

.

/%%EndObject
/end
a
 
gsave
newpath
86 $pos1 translate $rot1 3.0 3.0 scale ^eiscatlogo
13 -3 translate .9 .9 scale ^engaddress
grestore
.
w
q
!
