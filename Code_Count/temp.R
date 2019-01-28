temp.fixer = function(x){
  
  temp = as.numeric(x[which(names(x) == "water_temp")])
  
  if(is.na(temp)){
    return(NA)
  } 
  
  if(temp >= 60){ # convert to celsius
    temp = (temp - 32) * 5/9
  }
  
  if(temp <= 5){
    temp = NA
  }
  temp
}


stripper = function(x){
  trip.id = x[which(names(x) == "trip_id")]
  
  notes = x[which(names(x) == "sample_notes")]
  
  temp = NA
  
  if(trip.id == "GC20180915"){
    tmp = regexpr("backwater temperature = ", notes)
    tmp2 = regexpr("mainstem temperature = ", notes)
    
    idx.st = tmp + attr(tmp, 'match.length')
    idx.sp = tmp2 - 3
    
    temp = substr(notes, idx.st, idx.sp)
  }
  as.numeric(temp)
}


test = apply(sa4,1,stripper)  
test = apply(sa4,1,temp.fixer)


out = rep(NA, length = dim(sa4)[1])


for(i in 1:nrow(sa4)){
  x = sa4[i,]$water_temp
  
  if(is.na(x)){
    next
  }
  
  # if(sa4[i,]$trip_id == "GC20180915"){
  #  out[i] = sa4[i,]$water_temp
  #  next
  # }
  #
  # if(sa4[i,]$trip_id == "GC20170915"){
  #  out[i] = sa4[i,]$water_temp
  #  next
  # }
  
  out[i] = x
  
  if(x >= 60){  # convert to celsius
    out[i] = (x - 32) * 5/9
  }
  
  if(x < 5){
    out[i] = NA
  }
  
  
}

sa4$temp_bkw = out
  