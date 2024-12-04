* This program gathers location data from visit.dbf in the standard quack 
* format (date,time,easting, northing,check,nextvisit). These data are placed in 
* gdfile.dbf.  A series of criteria are used to eliminate duplicates and inactive nests.
* The data are reformatted for use with gardownd, copied to a text file and
* sent to the Garmin GPS using gardownd
* v1 - jbg - 07.05.2000
USE GDFILE
ZAP
CLEAR
answer = 'D'
@ 10,10 SAY 'DO YOU WANT ALL ACTIVE NESTS (A) OR JUST DAILY (D)'
@ 10,61 GET ANSWER PICTURE "@A!M A,D"
read
if answer = 'A'
APPEND FROM VISIT
* last record for each nest is .not. check 
* destroyed nests have next visit = '1/1/YYYY'
* gardown chokes if the easting or northing is 0 or has too few digits
* this might be a problem at some locations and could be fixed by adding
* trailing 0s to the text placed in the field zone.
DELE FOR CHECK .OR. easting < 100000 .or. northing <1000000 ;
   .OR. nextvisit = ctod('1/1/'+str(year(nextvisit)))
PACK
else
append from daily
endif
SET ORDER TO UNEST
* this reformats in a heavy-handed way - the value 'W' in t indicates a waypoint
* zone is the UTM zone, F is also a regional value, the rest is std reformating
REPL ALL T WITH 'W',NAME WITH NEST_NO,ZONE WITH '03', F WITH 'V 0',;
   day with  left(cdow(date),3), mon with left(cmonth(date),3),;
   dat with right(str( day(date)),3), t1 with '00:00:00',yr with year(date),;
   d2 with right(str(day(date)),2)+'-'+left(cmonth(date),3)+'-'+right(str( year(date)),2)

* reformats t2 with the time of the nest visit - this tag appears on the waypoint screen
repl for time > 100 t2 WITH left( ltrim(str(time)),2)+':'+right( ltrim(str(time)),2)
repl for time < 100 t2 WITH '00:'+right( ltrim(str(time)),2)
repl for time = 0 t2 WITH '00:00' 
* spits out the sdf text file - sp is a dummy variable containing a blank space
copy to  gdtmp.txt fields t,name,zone,f,easting,northing,sp,day,mon,dat,sp,t1,yr,sp,d2,sp,t2 sdf
* appends the gdhd.txt header file to the gdout.txt file created
* gdhd.txt contains info on the coordinate format and map datum for the waypoints
* - it must correspond to the GPS settings used when the waypoints were collected.
*run copy gdhd.txt+gdtmp.txt gdout.txt
*run copy wgshd.txt+gdtmp.txt gdout.txt
run copy hockpt.way+gdtmp.txt gdout.txt
* starts the send to the GPS - ALL EXISTING WAYPOINTS will be deleted!!!!
run gardownd -u gdout.txt
RETURN