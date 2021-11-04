* standardize names
gen str100 name=strupper(candidate) if bye==0
sort bye name

* split name into name1, name2, name3; name1 is the name stripped of trailing titles and commments
split name if bye==0,parse(,)

* create lastname from name1
gen lastname=word(name1,-1) if bye==0
gen lname2=word(name1,-2) if bye==0
replace lastname=lname2 + " " + lastname if lastname=="I"|lastname=="II"|lastname=="III"
drop lname2

* create name1n from name1 that does not have leading titles
* save standardized leading titles in variable ltitle
* create fullname (standardized full name, without trailing titles and comments)
gen name1n=name1
gen firstword=word(name1,1)
gen str10 ltitle=""
replace name1n=substr(name1,7,.) if firstword=="(HON.)"
replace ltitle="HON." if firstword=="(HON.)"
replace name1n=substr(name1,5,.) if firstword=="HON."
replace ltitle="HON." if firstword=="HON."
replace name1n=substr(name1,7,.) if firstword=="(LORD)"
replace ltitle="LORD" if firstword=="(LORD)"
replace name1n=substr(name1,5,.) if firstword=="LORD"
replace ltitle="LORD" if firstword=="LORD"
replace name1n=substr(name1,6,.) if firstword=="(SIR)"
replace ltitle="SIR" if firstword=="(SIR)"
replace name1n=substr(name1,4,.) if firstword=="SIR"
replace ltitle="SIR" if firstword=="SIR"

gen fullname=ltitle + name1n
drop firstword ltitle

gen fullname2 = fullname
replace fullname2=fullname + "," + name2 if name2!=""

* egen nf=sum((fullname2!=fullname2[_n-1])),by(name1n)
 
* candidates with the same fullname2 are likely the same person
* some candidates changed their names enough so that it is hard to track them

egen ngenelecs=sum(1),by(fullname2)
label var ngenelecs "n of general elections this guy contested"
ta ngenelecs 

egen firstyear = min(year*(1-bye)),by(fullname2)
label var firstyear "first general election year this guy contested"
ta firstyear

egen nconstits=sum(1-bye),by(fullname2 year)
ta nconstits

* drop name name1 name2 name3 lastname fullname ngenelecs firstyear nconstits