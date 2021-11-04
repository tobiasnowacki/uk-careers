* assign new member ids based on name_on_return
egen mid = group(name_)

sort mid year
egen seqno = seq(),by(mid)
replace seqno=. if mid==.
replace seqno=. if name_==""

ta year if seqno==1,s(winner)

egen meanwinys=mean(winner),by(year seqno)
sort year seqno
sc meanwinys year if seqno==1,c(L) xline(1832 1867 1884)

/* by party2 (currently only possible for post 1832)
egen meanwin2=mean(winner),by(year party2 seqno)
sc meanwin2 year if year>=1832&seqno==1&party2=="C",c(L) msymbol(Oh) xline(1867 1884) || sc meanwin2 year if year>=1832&seqno==1&party2=="L",c(L) 



/* there were problems:  in 1802, a name_ first appearing in the data
is not necessarily running for the first time.  This biases the win rate up
since a bunch of seqno > 1 are incorrectly identified as seqno == 1.  Recoding
the bye elections and fixing the names in hopt4 should have helped.
