CLEAR ALL
CLOSE ALL

SET TALK ON
SET ESCAPE ON
c:
cd\quack2000
USE VISIT 
REINDEX
SET ORDER TO PHOTO
SET FILTER TO (NEXTVISIT>CTOD('01/01/2001').AND. .NOT. CHECK) .AND.(NEXTVISIT < DATE()+1) 
SET SAFETY OFF
COPY TO c:\QUACK2000\DAILY FOR SPECIES='RTLO' .OR. SPECIES='PALO' .OR. SPECIES='UNLO'
*COPY TO C:\QUACK\DBROOD FOR PHOTO=999
SET SAFETY ON
 USE c:\QUACK2000\DAILY
*REPO FORM DAILY
COUNT
copy fields nest_no, nextvisit, species, status, no_eggs, candle1, candle2, comments, easting, northing delimited with blank to daily.txt

*copy fields nest_no, space, nextvisit,space, status,space, no_eggs,space, candle1,space, candle2,space, comments,space, easting,space, northing to shit.txt sdf
*set headings off

*ACCEPT 'LIST TO PRINTER? ' TO prt       
*DO WHILE LEFT(UPPER(prt),1) = 'Y'
*set printer to
*set printer on
*list off fields nest_no, nextvisit, status, no_eggs, candle1, candle2, comments, easting, northing to printer
*ACCEPT 'PRINT ANOTHER COPY?' TO prt
*IF LEFT(UPPER(prt),1)='Y'
*list off fields nest_no, nextvisit, status, no_eggs, candle1, candle2, comments, easting, northing to printer
*endif
*ENDDO
*quit
RETURN