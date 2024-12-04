use visit
replace all hatch with ctod('01/01/2003') 
set filter to hatch=ctod('01/01/2003') .and. no_eggs>0 .and. status<>'HATCHED'
replace all hatch with date+(26-(val(candle1)+val(candle2))/2)
replace all hatch with date+3 for float1='FA'
replace all hatch with date+6 for float1='FO'
replace all hatch with date+9 for float1='FB'
replace all hatch with date+12 for float1='FS'
replace all hatch with date+15 for float1='FN'
replace all hatch with date+18 for float1='SP'
replace all hatch with date+21 for float1='SO'
replace all hatch with date+24 for float1='SA'
