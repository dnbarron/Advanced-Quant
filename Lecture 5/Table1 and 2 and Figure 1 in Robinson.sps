get file='c:\robinson/table10.sav' .

**** Calculate totals for whites.
compute totalwhiteIL= nwhitenpop10IL+nwhitefpop10IL+fbwhitepop10IL.
compute totalwhitepop = nwhitenpop10+nwhitefpop10+fbwhitepop10.

**** Because Robinson has division as unit of analysis the data have to be aggregated
       to produce Table 1/ Table 2 / Figure 1.

AGGREGATE 
  /OUTFILE='c:\robinson\aggr1.sav' 
  /BREAK=division 
  /negropop=SUM(negropop10) 
  /negropopIL=SUM(negropop10IL) 
  /totalwhiteIL= SUM(totalwhiteIL) 
  /totalwhitepop=SUM(totalwhitepop). 
  
get FILE='c:\robinson\aggr1.sav'. 

*** Robinson uses per division figures in thousands. 
Compute negroLIT=rnd((negropop - negropopIL)/1000).
Compute negroIL=rnd (negropopIL / 1000).

Compute whiteLIT=rnd((totalwhitepop - totalwhiteIL)/1000).
Compute whiteIL=rnd(totalwhiteIL / 1000).

*** check om interiors, should be equal to Table 1 interiors.
des var=negroLIT whiteLIT negroIL whiteIL/ stat=sum.

* Percentage illiterate and percentage Blacks. 
Compute percILL=(whiteIL + negroIL) / (whiteLIT + negroLIT + whiteIL + negroIL) * 100.
Compute percnegro =  (negroLIT + negroIL)  / (whiteLIT + negroLIT + whiteIL + negroIL) * 100.

*** Figure 1!.
 GRAPH
  /SCATTERPLOT(BIVAR)=percnegro WITH percILL.

*** Robinson used weighted ecological correlations here!.
Compute number= (whiteLIT + negroLIT + whiteIL + negroIL) * 1000.
weight by number.
*** ecological correlation between percentage illiterate and percentage negroes  (Robinson: .946 we .946).
cor var=percnegro WITH percILL.

******************************************************************************************************************************.

** Individual analyses.
Loop x= 1 to 4.

if x=1  white = 1.
if x=1  literate = 0.
if x=1 number = whiteIL.

if x=2  white = 0.
if x=2  literate = 0.
if x=2 number = negroIL.

if x=3  white=1.
if x=3  literate=1.
if x=3 number=    whiteLIT.

if x=4  white=0.
if x=4  literate=1.
if x=4 number=  negroLIT.

value labels white literate 0 'no' 1  'yes'.

xsave outfile='c:\robinson/individual1.sav' / keep  division white literate number. 

End loop.
EXECUTE .

get file='c:\robinson/individual1.sav' .

Compute number2=number  * 1000.
weight by number2.

*** Table 1.
*** Individual correlation (Robinson .203 we .202).
CROSSTABS
  /TABLES=literate BY white
  /STATISTICS=Btau
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.

*** Table 2.
CROSSTABS
  /TABLES=literate BY white By division
  /STATISTICS=Btau
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.


