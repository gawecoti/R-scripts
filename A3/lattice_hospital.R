lattice_hospital = function() { 
  xyplot(death ~ npatient | owner,
         panel=function(x,y,...){
           panel.xyplot(x,y,...)
           panel.lmline(x,y,col=2)
           panel.text("text")
         },xlab="Number of Patients Seen", ylab="30-day Death Rate",main="Heart Attack 30-day Death Rate By Ownership")
}