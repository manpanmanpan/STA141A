### load packages
library(stringr)
### 1. read_post
read_post = function(file){
  ### INPUT: a text file
  ### OUTPUT: character string of text
  txt = readLines(file, encoding = 'UTF-8')
  return(txt)
}

### 2. read_all_posts
get_price = function(txt){
  ?## INPUT: character string of text
  ### OUTPUT: user-specified price 
  pr_att = str_extract(txt, 'Price: \\$[0-9,.]+')
  pr_att1 = pr_att[!is.na(pr_att)]
  ### handle error if any
  tryCatch({price = str_split_fixed(pr_att1, '\\$', 2)[1, 2]}, 
          ?error = function(err){price <<- NA})
  return(price)
}

get_title = function(txt){
  ### INPUT: character string of text
  ### OUTPUT: title
  title = txt[[1]]
  return(title)
}

get_text = function(txt){
  ### INPUT: character string of text
  ### OUTPUT:?text of posts
  ### locate the end of text
  end_index = str_which(a,'Date Posted: ') - 1
  text = str_trim(str_c(a[2:end_index], collapse = '\n'))
  return(text)
}

get_date_po = function(txt){
  ### INPUT: character string of text
  ### OUTPUT: date post?d 
  da_att = str_extract(txt, 'Date Posted:.*')
  da_att1 = da_att[!is.na(da_att)]
  ### handle error if any
  tryCatch({date_p = str_split_fixed(da_att1, 'Posted: ', 2)[1, 2]}, 
           error = function(err){date_p <<- NA})
  return(date_p)
}

get_lat?= function(txt){
  ### INPUT: character string of text
  ### OUTPUT: latitude 
  lat = str_extract(txt, 'Latitude: [0-9.-]+')
  lat1 = lat[!is.na(lat)]
  ### handle error if any
  tryCatch({latitude = str_split_fixed(lat1, 'Latitude: ', 2)[1, 2]}, 
       ?  error = function(err){latitude <<- NA})
  return(latitude)
}

get_long = function(txt){
  ### INPUT: character string of text
  ### OUTPUT: longitude 
  long = str_extract(txt, 'Longitude: [0-9.-]+')
  long1 = long[!is.na(long)]
  ### handle error if an??  tryCatch({longitude = str_split_fixed(long1, 'Longitude: ', 2)[1, 2]}, 
           error = function(err){longitude <<- NA})
  return(longitude)
}

get_bed = function(txt){
  ### INPUT: character string of text
  ### OUTPUT: number of bedrooms 
  bed = s??_extract(txt, 'Bedrooms: [0-9.]+')
  bed1 = bed[!is.na(bed)]
  ### handle error if any
  tryCatch({bedrooms = str_split_fixed(bed1, 'Bedrooms: ', 2)[1, 2]}, 
           error = function(err){bedrooms <<- NA})
  return(bedrooms)
}

get_bath = function(txt){?  ### INPUT: character string of text
  ### OUTPUT: number of bathrooms 
  bath = str_extract(txt, 'Bathrooms: [0-9.]+')
  bath1 = bath[!is.na(bath)]
  ### handle error if any
  tryCatch({bathrooms = str_split_fixed(bath1, 'Bathrooms: ', 2)[1, 2]}, 
      ?  error = function(err){bathrooms <<- NA})
  return(bathrooms)
}

get_sqft = function(txt){
  ### INPUT: character string of text
  ### OUTPUT: size of apartments
  size = str_extract(txt, 'Sqft: [0-9.]+')
  size1 = size[!is.na(size)]
  ### handle error?if?any
  tryCatch({sqft = str_split_fixed(size1, 'Sqft: ', 2)[1, 2]}, 
           error = function(err){sqft <<- NA})
  return(sqft)
}

get_allinfo = function(txt){
  ### INPUT: character string of text
  ### OUTPUT: vector of infomation about apartments 
  t?tle = get_title(txt)
  text = get_text(txt)
  latitude = get_lat(txt)
  longitude = get_long(txt)
  date_posted = get_date_po(txt)
  price = get_price(txt)
  sqft = get_sqft(txt)
  bedrooms = get_bed(txt)
  bathrooms = get_bath(txt)
  all_info = c(title, t?xt, latitude, longitude,
               date_posted, price, sqft, bedrooms, bathrooms)
  return(all_info)
}

read_all_posts = function(dir){
  ### INPUT: a post directory
  ### OUTPUT: dataframe of some information about posts
  files = list.files('./me?sy?sacramento/', full.names = TRUE)
  desc = sapply(files, read_post)
  info = sapply(desc, get_allinfo)
  data_frame = as.data.frame(t(info))
  rownames(data_frame) = NULL
  colnames(data_frame) = c('title', 'text', 'latitude', 'longitude',
              ?  ?         'date_posted', 'price', 'sqft', 'bedrooms', 'bathrooms')
  ### adjust Rtypes
  data_frame$title = as.character(data_frame$title)
  data_frame$text = as.character(data_frame$text)
  data_frame$latitude = as.numeric(as.character(data_frame$latitu?e)?
  data_frame$longitude = as.numeric(as.character(data_frame$longitude))
  data_frame$date_posted = as.character(data_frame$date_posted)
  data_frame$price = as.numeric(as.character(data_frame$price))
  data_frame$sqft = as.numeric(as.character(data_fra?e$?qft))
  data_frame$bedrooms = as.numeric(as.character(data_frame$bedrooms))
  data_frame$bathrooms = as.numeric(as.character(data_frame$bathrooms))
  return(data_frame)
}


###5
### define a fcn to transform character into numeric
to_numeric = function(cha?){
  a= str_remove_all(char, '[$,]')
  return(as.numeric(a))
}
###temp = str_extract_all(txt, 
###       regex('[\n.-][^.\n-]*deposit[^.\n-]*[\n.-]?', ignore_case = TRUE), simplify = T)
###define a fcn to get deposit when ncol(temp) = 1
get_de_temp1 = func?ion(temp){
  ### Only work when ncol(temp) = 1
  ### INPUT: string
  ### OUTPUT: infomation about deposit 
  if(str_detect(temp[1, 1], '\\$[0-9,.]+')){
    temp_1 =  str_extract_all(temp[1, 1], '\\$[0-9,.]+', simplify = T)
    final_goal = sapply(temp_1[1,?], to_numeric)
    return(max(final_goal))
  }else if(str_detect(temp[1, 1], regex('(no deposit|zero deposit|not deposit)', ignore_case = T))){
    return(0)
  }else if(str_detect(temp[1, 1], regex('(one month|1 month)', ignore_case = T))){
    return('one?month')
  }else if(str_detect(temp[1, 1], regex('(½ a month|half (a )?month)', ignore_case = T))){
    return('1/2 month')}else {return(NA)}
}

get_depos = function(txt){
  ### Work for all situation in this dataset
  ### INPUT: string
  ### OUTPUT: infoma?ion about deposit
  temp = str_extract_all(txt, 
                         regex('[\n.-][^.\n-]*deposit[^.\n-]*[\n.-]?', ignore_case = TRUE), simplify = T)
  if (ncol(temp) == 0){return(NA)}
  if (ncol(temp) == 1){
    return(get_de_temp1(temp))
  } else {f?r(j in 1: ncol(temp)){
    if(str_detect(temp[1, j], regex('pet|dog|cat', ignore_case = T))){
      next}else if(is.na(get_de_temp1(matrix(temp[1, j])))){
        next
      }else {return(get_de_temp1(matrix(temp[1, j])))}
  }}
  return(NA)
}
depo_re = sap?ly(cl_all$text, get_depos, USE.NAMES = F)
### combind depo_re into dataframe
depo_re
cl_all$deposit = depo_re
cl_all$deposit[which(cl_all$deposit == 'one month')] = cl_all$price[which(cl_all$deposit == 'one month')]
cl_all$deposit[which(cl_all$deposit == '?/2 month')] = 0.5 * cl_all$price[which(cl_all$deposit == '1/2 month')]
cl_all$deposit = as.numeric(cl_all$deposit)
### finished!
###make plot 








getContext <- function(text, look_for, pre = 3, post=pre) {
  # create vector of words (anything separate? by a space)
  t_vec <- unlist(strsplit(text, '\\s'))
  
  # find position of matches
  m <- which(t_vec==look_for)
  
  # return words before & after if any matches
  if(length(m) > 0) {
    out <- 
      list(before = ifelse(m-pre < 1, NA, 
             ?             sapply(m, function(m) t_vec[(m - pre):(m - 1)])), 
           after = sapply(m, function(m) t_vec[(m + 1):(m + post)]))
    
    return(out)
  } else {
    warning('No matches')
  }
}



