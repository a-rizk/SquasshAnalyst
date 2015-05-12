library(shiny)

source("package_handling.R") 
libraries <- c("VennDiagram","gtools","zoo")
print(libraries)
for(library in libraries) 
{ 
  #print("lib server")
  #print(library)
  if(!UsePackage(library))
  {
    stop("Error!", library)
  }
}


####### directory selection mac os
choose_dir_mac <- function() {
  system("osascript -e 'tell application \"System Events\" to set frontApp to name of first application process whose frontmost is true' -e 'tell application frontApp to POSIX path of (choose folder with prompt \"Chosse folder containing Squassh Data:\")' > /tmp/R_folder", 
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}

#if(is.na(a)) stop("No folder", call. = F)

onwindows <<- grepl("windows", .Platform$OS.type, fixed=TRUE)
onmacos <<- grepl("Darwin", Sys.info()["sysname"], fixed=TRUE)

#print(onmacos)


#-e 'set frontApp to name of first application process whose frontmost is true'



#libraries <- c("VennDiagram","tiff")



####################################################################################
############## DATA INPUT
####################################################################################

#construct vector containing files of a condition
temp_condition_vector <- function(img_data_file_path, ch1_files, ch2_files, ch3_files){
  condname= sub(".*/","",img_data_file_path,fixed = TRUE) #get file name
  condname= sub("ImagesData.csv","",condname,fixed = TRUE) #remove ending
  #search for c1, c2 ,c3 files
  c1=ch1_files[grepl(condname, ch1_files, fixed = TRUE)]
  c2=ch2_files[grepl(condname, ch2_files, fixed = TRUE)]
  c3=ch3_files[grepl(condname, ch3_files, fixed = TRUE)]
  res=img_data_file_path
  
  if (length(c1)==0)return(res)
  res=c(res,head(c1,1)) #if more than one data file for channel, not well defined take first by default
  
  if (length(c2)==0)return(res)
  res=c(res,head(c2,1))
  
  if (length(c3)==0)return(res)
  res=c(res,head(c3,1))
  
  return(res)
}

#return ture if data is empty
empty_data <- function(data_list){
  
  return(is.null(data_list) | length(data_list)==0)
}


#load condition data and put it in a vector, take as argument vector of file paths of one condition
#c("short condition name", floder path, script_params, datafarame_imgs, dataframe_c1, dataframe_c2, dataframe_c3)
load_condition_data<-function(paths){
  len=length(paths)
  #get file name and directory path
  file_name = sub(".*/","", paths[1]) #get file name
  dir_path = sub(file_name,"", paths[1],fixed = TRUE)
  #print(file_name)
  short_name = sub("ImagesData.csv","",file_name,fixed = TRUE) #remove ending
  short_name = sub("_*ZStacks_*","",short_name) #remove  Zstacks
  short_name = sub("_*extracted_*","",short_name) #remove  Zstacks
  short_name = sub("_*zstacks_*","",short_name) #remove  Zstacks
  #print(short_name)
  short_name = sub("_$","",short_name) #remove  '_' ending if still present (2D images)
  res = c(short_name,dir_path)
  
  #read img average data
  data_all=read.csv(paths[1], sep = ";")
  nr_all=nrow(data_all)
  #extract squassh params
  param_line=as.character(data_all[nr_all,1])
  param_line=sub("Min intensity ch1", "\nMin intensity ch1", param_line,fixed = TRUE)
  param_line=sub("Cell mask ch2", "\nCell mask ch2", param_line,fixed = TRUE)
  param_line=sub("Parameters", "Plugin parameters", param_line,fixed = TRUE)
  
  #remove param line in data frame
  data_all=data_all[seq(1,nr_all-1,1),]
  
  #add to res
  res=c(res,param_line,list(data_all))
  
  
  for(i in 2:len){
    data=read.csv(paths[i], sep = ";")
    res=c(res,list(data))
  }
  
  return(res)
}

#create data list, each element is a list of form given by load_condition_data
load_all_condition_data <- function(img_data_files, ch1_data_files, ch2_data_files, ch3_data_files){
  
  #generate temp
  cond_list=lapply(img_data_files, temp_condition_vector, ch1_files=ch1_data_files, ch2_files=ch2_data_files, ch3_files=ch3_data_files)
  if(length(cond_list)==0){return(cond_list)}

  max_channels <<- max(sapply(cond_list,length)) -1 # to remove conditions wihth less than max channels, necessary ?
  #remove conditions with less than one channel :
  cond_list=cond_list[sapply(cond_list,length)>=2]

  #load all data in data_list
  data_list<-lapply(cond_list, load_condition_data)
  
  nbconditionsv <<- length(data_list)
  
  return(data_list)
}

load_data <- function(selected_path) {

  setwd(selected_path)
  print(getwd())
  selected_path<<-selected_path
  img_data_files = list.files(path=selected_path, pattern= "ImagesData.csv$", recursive = TRUE)  
  ch1_data_files = list.files(path=selected_path, pattern= "ObjectsData_c1.csv$", recursive = TRUE) # !double escape "\\."  escapes "."
  ch2_data_files = list.files(path=selected_path, pattern= "ObjectsData_c2.csv$", recursive = TRUE) 
  ch3_data_files = list.files(path=selected_path, pattern= "ObjectsData_c3.csv$", recursive = TRUE) 

  data_list = load_all_condition_data(img_data_files, ch1_data_files, ch2_data_files, ch3_data_files)
  
  #print("load result is")
  #print(load_result)
  
  return(data_list)
}


####data filtering
filter_data <- function(data_list, include_matrix, max_sizes, min_sizes, min_intensities, min_objects){
  
  nbconds = length(data_list)
  new_data=list()
  #print('filter')
  for(cond in 1:nbconds){
    nbchannels = length(data_list[[cond]])-4
    nbimgs=nrow(data_list[[cond]][[4]])
    #conditions infos
    new_data[[cond]]=list()
    new_data[[cond]][[1]]=data_list[[cond]][[1]]
    new_data[[cond]][[2]]=data_list[[cond]][[2]]
    new_data[[cond]][[3]]=data_list[[cond]][[3]]
    #create include list
    incl_list=NULL
    for(i in 1:nbimgs){
      #print("la")
      #print(i)
      #print(cond)
      if(include_matrix[i,cond])incl_list=c(incl_list,i-1)
      #print("la2")
    }
    
    #conditions dataframes
    #img data
    imgdat=data_list[[cond]][[4]]
    new_data[[cond]][[4]]=imgdat[imgdat$Image.ID %in% incl_list,]
    #remove data if not enough objects in channel 1, channel 2 or channel 3
    if(!is.null(imgdat$Objects.ch1)){
      new_data[[cond]][[4]]=new_data[[cond]][[4]][new_data[[cond]][[4]]$Objects.ch1 >= min_objects[1],]
    }
    if(!is.null(imgdat$Objects.ch2)){
      new_data[[cond]][[4]]=new_data[[cond]][[4]][new_data[[cond]][[4]]$Objects.ch2 >= min_objects[2],]
    }
    if(!is.null(imgdat$Objects.ch3)){
      new_data[[cond]][[4]]=new_data[[cond]][[4]][new_data[[cond]][[4]]$Objects.ch3 >= min_objects[3],]
    }
    
    #update include list
    incl_list=new_data[[cond]][[4]]$Image.ID
      #C1
    if(!is.null(imgdat$Objects.ch1)){
      c1dat=data_list[[cond]][[5]]
      c1dat=c1dat[c1dat$Image.ID %in% incl_list,]
      c1dat=c1dat[c1dat$Size >= min_sizes[1] & c1dat$Size <= max_sizes[1] & c1dat$Intensity >= min_intensities[1],]
      new_data[[cond]][[5]]=c1dat
    }
    
    if(!is.null(imgdat$Objects.ch2)){
      c2dat=data_list[[cond]][[6]]
      c2dat=c2dat[c2dat$Image.ID %in% incl_list,]
      c2dat=c2dat[c2dat$Size >= min_sizes[2] & c2dat$Size <= max_sizes[2] & c2dat$Intensity >= min_intensities[2],]
      new_data[[cond]][[6]]=c2dat
    }
    
    if(!is.null(imgdat$Objects.ch3)){
      c3dat=data_list[[cond]][[7]]
      c3dat=c3dat[c3dat$Image.ID %in% incl_list,]
      c3dat=c3dat[c3dat$Size >= min_sizes[3] & c3dat$Size <= max_sizes[3] & c3dat$Intensity >= min_intensities[3],]
      new_data[[cond]][[7]]=c3dat
    }
    
  }
  
  
  new_data
  
}



####################################################################################
############## Features computation
####################################################################################

#feature selection

feature_computation <- function(type, data, channel, overlapChannel, max_sizes, min_sizes, min_intensities, condnames){
  switch(type,
         "Mean object volume" = mean_ves_size(data,channel,condnames),
         "Mean object length" = mean_ves_length(data,channel,condnames),
         "Mean object intensity" = mean_ves_intensity(data,channel,condnames),
         "Pearson correlation" = pearson_corr(data,channel,condnames),#two channels
         "Pearson correlation inside cell mask" = pearson_corr_in_mask(data,channel,condnames),#two channels
         "Colocalization (number)" = 100*coloc(data, channel, overlapChannel, max_sizes, min_sizes, min_intensities,condnames),
         "Colocalization (size)" = 100*colocSize(data, channel, overlapChannel, max_sizes, min_sizes, min_intensities,condnames),
         "Colocalization (signal)" = 100*colocSignal(data, channel, overlapChannel, max_sizes, min_sizes, min_intensities,condnames),
         "Total object signal" = total_ves_signal(data,channel,FALSE,condnames),
         "Total object signal / Cell Size" = total_ves_signal(data,channel,TRUE,condnames),
         "Total object volume" = total_ves_volume(data,FALSE,channel),
         "Total object volume / Cell Size" = total_ves_volume(data,TRUE,channel),
         "Object number" = mean_ves_number(data,channel,FALSE,condnames),
         "Object number / Cell Size" = mean_ves_number(data,channel,TRUE,condnames)
  )
}




#return a matrix with one column per condition and one line per image
mean_ves_size = function(data_list,channel,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  #print(channel)
  channel_column = channel+4
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      colves=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      r=mean(colves$Size,na.rm =TRUE)
      if(is.nan(r))r=0
      res[k,cond]=r
      k=k+1
    }
  }
  
  return(res)
}

mean_ves_length = function(data_list,channel,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      colves=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
     
      r=mean(colves$Length,na.rm =TRUE)
      if(is.nan(r)) r=0
      res[k,cond]=r
      k=k+1
    }
  }
  
  return(res)
}

mean_ves_intensity = function(data_list,channel,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      colves=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      
      r=mean(colves$Intensity,na.rm =TRUE)
      if(is.nan(r)) r=0
      res[k,cond]=r
      k=k+1
    }
  }
  
  return(res)
}
# 
# mean_ves_sphericity = function(data_list,channel,condnames){
#   #create variables...
#   NC = length(data_list) # number of conditions
#   res=matrix(nrow=max(max_imgs),ncol=NC)
#   channel_column = channel+4
#   
#   #loop on conditions and images
#   for(cond in 1:NC){
#     imgids = data_list[[cond]][[4]][,"Image.ID"]
#     k=1
#     for( imgid in imgids ){
#       incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
#       cond_ch_df=data_list[[cond]][[channel_column]]
#       colves=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
#       
#       r=mean(colves$Intensity,na.rm =TRUE)
#       if(is.nan(r)) r=0
#       res[k,cond]=r
#       k=k+1
#     }
#   }
#   
#   return(res)
# }

#################
#Vesicle sphericity
#################
mean_ves_sphericity = function(dtl, NR){
  #create variables...
  #rd=matrix(nrow=NC,ncol=NR)
  m=1
  s=1
  sum=0
  #number of vesicles
  for(rab in 1:NR){
    rd=matrix(nrow=NC2[rab],ncol=1)
    for( i in 1:NC2[rab] ){
      colves=(dtl[[rab]][dtl[[rab]]$Image.number==(sum+(i-1)),])
      
      #remove zeros in Perimeter column (due to probleme in code, corrected now)

      
      
      rd[i,1]=mean((pi**(1/3))*((6*(colves$Size))**(2/3))/(colves$Perimeter))
      
    }
    
    sum=sum+NCR[rab]
    m[rab]=mean(rd[,1])
    s[rab]=sd(rd[,1])/(sqrt(length(rd[,1])))
  }
  
  
  
  result = data.frame(mean=m,
                      sem=s)
  result
  
}



total_ves_signal = function(data_list,channel,norm,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      img_df=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      
      n1=nrow(img_df)
      tInt=0
      
      if(n1!=0){
      
        for(ves in 1:n1){
      
            
          tInt=tInt+ img_df[ves,'Size'] * img_df[ves,'Intensity']
        }
        resImg=tInt
      }
      else{
        resImg=0
      }
      res[k,cond]=resImg
      
      if(norm)
      {
        dat = data_list[[cond]][[4]]
        cellSize= dat[dat$Image.ID==imgid,'Cell.Mask.Size']
        res[k,cond]=resImg/cellSize
      }
      
      k=k+1
    }
  }
  
  return(res)
}


total_ves_volume = function(data_list,norm,channel){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  #print(data_list)
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      cond_ch_df=data_list[[cond]][[channel_column]]
      img_df=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      
      n1=nrow(img_df)
      tInt=0
      
      if(n1!=0){
        
        for(ves in 1:n1){
          tInt=tInt+ img_df[ves,'Size']
        }
        resImg=tInt
      }
      else{
        resImg=0
      }
      res[k,cond]=resImg
      
      if(norm)
      {
        dat = data_list[[cond]][[4]]
        cellSize= dat[dat$Image.ID==imgid,'Cell.Mask.Size']
        res[k,cond]=resImg/cellSize
      }
      
      k=k+1
    }
  }
  
  return(res)
}


total_ves_volume_multi = function(data_list, channel, overlapChannel, max_sizes, min_sizes, min_intensities){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  #print('multi')
  #print(channel)
  #print(overlapChannel)
  
  if(length(overlapChannel)==1){
    overlapString = paste0("Overlap.with.ch",overlapChannel)
    if(max_chans == 2){
      colocSizeString="Coloc.object.size"
      colocIntString="Coloc.object.intensity"
    }
    else{
      colocSizeString=paste0("Coloc.object.size.ch",overlapChannel)
      colocIntString=paste0("Coloc.object.intensity.ch",overlapChannel)
    }
  }
  else{
    if(!(1 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch2ch3"
      colocIntString="Coloc.object.intensity.ch2ch3"
      overlapString = paste0("Overlap.with.ch2ch3")
    }
    if(!(2 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch1ch3"
      colocIntString="Coloc.object.intensity.ch1ch3"
      overlapString = paste0("Overlap.with.ch1ch3")
    }
    if(!(3 %in% overlapChannel)){ 
      colocSizeString="Coloc.object.size.ch1ch2"
      colocIntString="Coloc.object.intensity.ch1ch2"
      overlapString = paste0("Overlap.with.ch1ch2")
    }
  }
  
  #print(overlapString)
  #print(colocSizeString)
  
  maxSizeOv=max_sizes[channel]
  minSizeOv=min_sizes[channel]
  minIntOv=min_intensities[channel]
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      cond_ch_df=data_list[[cond]][[channel_column]]
      img_df=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      #print(img_df)
      n1=nrow(img_df)
      coInt=0
      
      if(n1!=0){
        ov=0
        for(ves in 1:n1){
          #print(img_df[ves,colocSizeString])
          if(img_df[ves,colocSizeString] > maxSizeOv || img_df[ves,colocSizeString] < minSizeOv || img_df[ves,colocIntString] < minIntOv ){
            ov=0
          }else{ ov = img_df[ves,overlapString]} #objects colocalizing with non valid objects are considered non colocalizing
          
          
          coInt=coInt + ov * img_df[ves,'Size']
        }
        resImg=coInt
      }
      else{
        resImg=0
      }
      
      res[k,cond]=resImg
      
      k=k+1
    }
  }
  
  #print(res)
  return(res)  
  
}







pearson_corr = function(data_list,channels,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  if(length(channels)==0)
    plabel="Pearson.correlation"
  else{
    if(!(1 %in% channels))
      plabel="Pearson.23"
    if(!(2 %in% channels))
      plabel="Pearson.13"
    if(!(3 %in% channels))
      plabel="Pearson.12"
  }
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[4]]
      pearson=cond_ch_df[cond_ch_df$Image.ID==imgid,plabel]
      
      res[k,cond]=pearson
      k=k+1
    }
  }
  
  return(res)
}

pearson_corr_in_mask = function(data_list,channels,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  if(length(channels)==0)
    plabel="Pearson.correlation.inside.cell.masks"
  else{
    if(!(1 %in% channels))
      plabel="Pearson.23.in.mask"
    if(!(2 %in% channels))
      plabel="Pearson.13.in.mask"
    if(!(3 %in% channels))
      plabel="Pearson.12.in.mask"
  }
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[4]]
      pearson=cond_ch_df[cond_ch_df$Image.ID==imgid,plabel]
      
      res[k,cond]=pearson
      k=k+1
    }
  }
  
  return(res)
}


mean_ves_number = function(data_list,channel,norm,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]      
      res[k,cond]=nrow(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      
      if(norm)
      {
        dat = data_list[[cond]][[4]]
        cellSize= dat[dat$Image.ID==imgid,'Cell.Mask.Size']
        res[k,cond]=res[k,cond]/cellSize
      }
      
      k=k+1
    }
  }
  
  return(res)
}

#Colocalization features
#object number based colocalization
#channel and overlapchannel are integers 
coloc = function(data_list, channel, overlapChannel, max_sizes, min_sizes, min_intensities,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  if(length(overlapChannel)==1){
    overlapString = paste0("Overlap.with.ch",overlapChannel)
    if(max_chans == 2){
      colocSizeString="Coloc.object.size"
      colocIntString="Coloc.object.intensity"
    }
    else{
      colocSizeString=paste0("Coloc.object.size.ch",overlapChannel)
      colocIntString=paste0("Coloc.object.intensity.ch",overlapChannel)
    }
  }
  else{
    if(!(1 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch2ch3"
      colocIntString="Coloc.object.intensity.ch2ch3"
      overlapString = paste0("Overlap.with.ch2ch3")
    }
    if(!(2 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch1ch3"
      colocIntString="Coloc.object.intensity.ch1ch3"
      overlapString = paste0("Overlap.with.ch1ch3")
    }
    if(!(3 %in% overlapChannel)){ 
      colocSizeString="Coloc.object.size.ch1ch2"
      colocIntString="Coloc.object.intensity.ch1ch2"
      overlapString = paste0("Overlap.with.ch1ch2")
    }
  }
  
  maxSizeOv=min(max_sizes[overlapChannel])
  minSizeOv=max(min_sizes[overlapChannel])
  minIntOv=max(min_intensities[overlapChannel])
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      img_df=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      
      #total number of vesicles
      n1=nrow(img_df)
      #colocalizing vesicles
      c1=nrow(img_df[ img_df[,overlapString]>0.5 & img_df$Coloc.object.intensity>minIntOv & img_df$Coloc.object.size<maxSizeOv & img_df$Coloc.object.size>minSizeOv,])
      
      
      if(n1!=0){
        res[k,cond]=c1/n1
      }
      else{
        res[k,cond]=0
      }
      k=k+1
    }
  }
  
  return(res)  
}

#coloc size based
colocSize = function(data_list, channel, overlapChannel, max_sizes, min_sizes, min_intensities,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  if(length(overlapChannel)==1){
    overlapString = paste0("Overlap.with.ch",overlapChannel)
    if(max_chans == 2){
      colocSizeString="Coloc.object.size"
      colocIntString="Coloc.object.intensity"
    }
    else{
      colocSizeString=paste0("Coloc.object.size.ch",overlapChannel)
      colocIntString=paste0("Coloc.object.intensity.ch",overlapChannel)
    }
  }
  else{
    if(!(1 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch2ch3"
      colocIntString="Coloc.object.intensity.ch2ch3"
      overlapString = paste0("Overlap.with.ch2ch3")
    }
    if(!(2 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch1ch3"
      colocIntString="Coloc.object.intensity.ch1ch3"
      overlapString = paste0("Overlap.with.ch1ch3")
    }
    if(!(3 %in% overlapChannel)){ 
      colocSizeString="Coloc.object.size.ch1ch2"
      colocIntString="Coloc.object.intensity.ch1ch2"
      overlapString = paste0("Overlap.with.ch1ch2")
    }
  }
  
  
  maxSizeOv=min(max_sizes[overlapChannel])
  minSizeOv=max(min_sizes[overlapChannel])
  minIntOv=max(min_intensities[overlapChannel])
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      img_df=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      #print(img_df)
      n1=nrow(img_df)
      coInt=0
      tInt=0
      
      if(n1!=0){
        ov=0
        for(ves in 1:n1){
          #print(img_df[ves,colocSizeString])
          if(img_df[ves,colocSizeString] > maxSizeOv || img_df[ves,colocSizeString] < minSizeOv || img_df[ves,colocIntString] < minIntOv ){
            ov=0
          }else{ ov = img_df[ves,overlapString]} #objects colocalizing with non valid objects are considered non colocalizing
          
          
          coInt=coInt + ov * img_df[ves,'Size']
          tInt=tInt+ img_df[ves,'Size']
        }
        resImg=coInt/tInt
      }
      else{
        resImg=0
      }
      
      res[k,cond]=resImg
      
      k=k+1
    }
  }
  
  return(res)  

}

#coloc signal based (size and Intensity)
colocSignal = function(data_list, channel, overlapChannel, max_sizes, min_sizes, min_intensities,condnames){
  #create variables...
  NC = length(data_list) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  channel_column = channel+4
  
  if(length(overlapChannel)==1){
    overlapString = paste0("Overlap.with.ch",overlapChannel)
    if(max_chans == 2){
      colocSizeString="Coloc.object.size"
      colocIntString="Coloc.object.intensity"
    }
    else{
      colocSizeString=paste0("Coloc.object.size.ch",overlapChannel)
      colocIntString=paste0("Coloc.object.intensity.ch",overlapChannel)
    }
  }
  else{
    if(!(1 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch2ch3"
      colocIntString="Coloc.object.intensity.ch2ch3"
      overlapString = paste0("Overlap.with.ch2ch3")
    }
    if(!(2 %in% overlapChannel)){
      colocSizeString="Coloc.object.size.ch1ch3"
      colocIntString="Coloc.object.intensity.ch1ch3"
      overlapString = paste0("Overlap.with.ch1ch3")
    }
    if(!(3 %in% overlapChannel)){ 
      colocSizeString="Coloc.object.size.ch1ch2"
      colocIntString="Coloc.object.intensity.ch1ch2"
      overlapString = paste0("Overlap.with.ch1ch2")
    }
  }
  
  maxSizeOv=min(max_sizes[overlapChannel])
  minSizeOv=max(min_sizes[overlapChannel])
  minIntOv=max(min_intensities[overlapChannel])
  
  #loop on conditions and images
  for(cond in 1:NC){
    imgids = data_list[[cond]][[4]][,"Image.ID"]
    k=1
    for( imgid in imgids ){
      incProgress(amount = 1/(NC*imgids), detail = condnames[cond])
      cond_ch_df=data_list[[cond]][[channel_column]]
      img_df=(cond_ch_df[cond_ch_df$Image.ID==imgid,])
      
      n1=nrow(img_df)
      coInt=0
      tInt=0
      
      if(n1!=0){
        ov=0
        for(ves in 1:n1){
          if(img_df[ves,colocSizeString] > maxSizeOv || img_df[ves,colocSizeString] < minSizeOv || img_df[ves,colocIntString] < minIntOv ){
            ov=0
          }else{ ov = img_df[ves,overlapString]} #objects colocalizing with non valid objects are considered non colocalizing
          
          
          coInt=coInt + ov * img_df[ves,'Size'] * img_df[ves,'Intensity']
          tInt=tInt+ img_df[ves,'Size'] * img_df[ves,'Intensity']
        }
        resImg=coInt/tInt
      }
      else{
        resImg=0
      }
      
      res[k,cond]=resImg
      
      k=k+1
    }
  }
  
  return(res)  
  
}




####################################################################################
##################     Statistical analysis       ##################################
####################################################################################

pvstr = function(pval){
  
  if(pval < 0.0001){pstring=sprintf("< 1e-4, ****" )}
  else if(pval < 0.001){pstring=sprintf("%.2e, ***",pval )}
  else if(pval < 0.01){pstring=sprintf("%.2e, **",pval )}
  else if(pval < 0.05){pstring=sprintf("%.2e, *",pval )}
  else{pstring= sprintf("%.2e, ns",pval )}
  
  return(pstring)
  
}


stat_aov_tukey= function(data, nbconds, condnames){

  var = as.vector(data)
  cond = rep (condnames,rep(max_imgs,nbconds))

  data2= data.frame(var, cond) 

  aovr = aov(var ~ cond , data2) #1 way anova 

  
  tuk = TukeyHSD(aovr)# tukey test
  
  pval = summary(aovr)[[1]][1,"Pr(>F)"]
  pstring=pvstr(pval)
  pstring= paste("Probability that all conditions have same mean (one way ANOVA p-value) :", pstring)
  #print(pstring)
  for (i in seq_along(tuk)) {
    xi <- 100*tuk[[i]][, -4, drop = FALSE]
    yvals <- nrow(xi):1
    ynames=dimnames(xi)[[1L]]
    ypvalues=sapply(tuk$cond[,4], pvstr)
    ylabs=paste0(ynames,", ", ypvalues)
  }

  return(c(pstring, "Pairwise comparisons (Tukey's HSD):",ylabs)) 
} 


tukeyplot100 = function (x, minp, maxp, xlabel)#, ylabel) 
{
  for (i in seq_along(x)) {
    xi <- 100*x[[i]][, -4, drop = FALSE]
    yvals <- nrow(xi):1
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), 
         type = "n", axes = FALSE, xlab = xlabel, ylab = "", main = "", xlim=c(100*minp,100*maxp))
    axis(1)
    ynames=dimnames(xi)[[1L]]
    ypvalues=sapply(x$cond[,4], pvstr)
    ylabs=paste0(ynames,", ", ypvalues)
    axis(2, at = nrow(xi):1, labels = ylabs, 
         srt = 0)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0.5)
    segments(xi[, "lwr"], yvals, xi[, "upr"], yvals)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3), as.vector(xi), 
             rep.int(yvals + 0.1, 3))
    # title(main = paste(format(100 * attr(x, "conf.level"), 
    #     digits = 2), "% family-wise confidence level\n", 
    #     sep = ""), xlab = paste("Differences in mean levels of", 
    #     names(x)[i]))
    box()
  }
}


####################################################################################
############## Plot functions
####################################################################################
sdfun <- function(vec){
  return(sd(vec,na.rm = TRUE)/(sqrt(sum( !is.na( vec ) ))))
}

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length,lwd=1, ...)
}

plotres=function(res,cnames,ymin,ymax,ylabel){
  means = apply(res,2,mean,na.rm = TRUE)
  sems = apply(res,2,sdfun)
  sems= sems
  ymax2=max(means)+max(sems)
  par(lwd = 1)
  par(mar= c(12,6.5,0.2,1)+0.1)
  barx <- barplot(means, names.arg=cnames,ylim=c(ymin,ymax2), 
                  axis.lty=1, 
                  ylab=ylabel, 
                  col="lightsteelblue1",
                  cex.lab=1.5,
                  cex.names=1,
                  las=2,
                  yaxt="n"
                  #par(mar= c(6,4,2,1)+0.1)
                  )
  axis(2,cex.axis=1.2)
  
  
  error.bar(barx,means, sems)
  #box("figure", lty="dotted", col = "blue")
}

plot_whisk=function(res,cnames,ymin,ymax,ylabel){
  #print(res)
  #par(mar= c(12,4.5,1,1)+0.1)
  #par(mar= c(6,4.5,0,1)+0.1)
  par(mar= c(12,6.5,0.2,1)+0.1)
  boxplot(res, names=cnames,ylim=c(ymin,ymax), axis.lty=1, ylab=ylabel,cex.axis=1,
          col="lightsteelblue1",
          cex.lab=1.5,
          yaxt="n",
          las=2,
          frame=F
  )
  axis(2,cex.axis=1.2)
}

strip_chart=function(res,cnames,ymin,ymax,ylabel){
  par(mar= c(12,6.5,0.2,1)+0.1)
  boxplot(res, names=cnames,ylim=c(ymin,ymax), axis.lty=1, ylab=ylabel,cex.axis=1,
          col="lightsteelblue1",
          outline=FALSE,
          #par(mar= c(6,4.5,2,1)+0.1),
          las=2,
          frame=F,
          cex.lab=1.5,
          cex.names=1.2,
          yaxt="n"
  )
  axis(2,cex.axis=1.2)
  
  stripchart(res, vertical=TRUE, add=TRUE, method = "jitter", pch = 21, col = "black", bg = "black", cex=1)
  
}


plot_movie=function(res,cnames,ymin,ymax,ylabel){
  #print(res)
  nc=ncol(res)
  par(mar= c(6,6.5,0.2,1)+0.1)
  plot(res[,1],type='n',
       ylim=c(ymin,ymax),
       #xlim=c(0,nrow(res)),
       #axis.lty=1, 
       ylab=ylabel,
       xlab="Time point",
       lwd=2,
       cex.axis=1.2,
       cex.sub=1.2,
       cex.lab=1.5,
      # par(mar= c(6,4.5,2,1)+0.1),
       yaxt="n"
       )
  for(i in 1:nc){
    lines(res[,i], lty=i, cex=0.7, lwd=2)#indianred2
  }
  #par(xpd=TRUE)
  #par(xpd=TRUE)

  if(nc>1){
    #legend('topleft', #inset=c(0,-0.1*nc),# places a legend at the appropriate place 
     #      cnames, # puts text in the legend 
      #     lty=1:nc, cex=1)  
  }
  
  
  axis(2,cex.axis=1.2)
  
  
  
}

reorder_movie_data = function(imgfeatures,filtereddata){
  
  NC = length(filtereddata) # number of conditions
  res=matrix(nrow=max(max_imgs),ncol=NC)
  
  
  for(cond in 1:NC){
    time_order= mixedorder(filtereddata[[cond]][[4]]$File)
    
    reorder=imgfeatures[time_order,cond]
    nbimgs =length(reorder)

    for( imgid in 1:nbimgs ){
      res[imgid,cond]=reorder[imgid]
    }
  }
  
  res
}


plot_features <- function(type,res,cnames,ymin,ymax,ylabel){
  #print(ylabel)
  switch(type,
         "Box plot" = plot_whisk(res,cnames,ymin,ymax,ylabel),
         "Strip chart" = strip_chart(res,cnames,ymin,ymax,ylabel),
         "Bar chart (Mean with SEM)" = plotres(res,cnames,ymin,ymax,ylabel)
  )
}


plot_venn_2chans = function(TS, ch1,ch2){
  TS1m=signif(TS[1],3)
  TS2m=signif(TS[2],3)
  TS12m=signif(TS[3],3)
  vennplot<-draw.pairwise.venn(
    TS1m, TS2m, TS12m,
    category=c(ch1, ch2),
    col = c("red", "green"),
    fill = c("red", "green"),
    alpha = 0.15,
    label.col = "black",
    cex = 1.35,
    fontfamily = "serif",
    fontface = "bold",
    cat.default.pos = "text",
    cat.col = c("darkred", "darkgreen"),
    cat.cex = 1.85,
    cat.fontfamily = "serif",
    cat.dist = c(0.06, 0.06),
    cat.pos = 0,
    ind =TRUE,
    scaled=TRUE,
    margin=0.05,
    euler.d=TRUE
  )
  
  grid.draw(vennplot)
  
}

plot_venn_3chans = function(TS,ch1,ch2,ch3){
  TS1m=signif(TS[1],3)
  TS2m=signif(TS[2],3)
  TS3m=signif(TS[3],3)
  TS12m=signif(TS[4],3)
  TS23m=signif(TS[5],3)
  TS13m=signif(TS[6],3)
  TS123m=signif(TS[7],3)
  #print(TS3m)
  #print(TS23m)
  
  
  vennplot<-draw.triple.venn(
    TS1m, TS2m, TS3m,TS12m, TS23m, TS13m, TS123m,
    category=c(ch1, ch2, ch3),
    col = c("red", "green", "blue"),
    fill = c("red", "green", "blue"),
    alpha = 0.15,
    label.col = "black",
    cex = 1.35,
    fontfamily = "serif",
    fontface = "bold",
    cat.default.pos = "text",
    cat.col = c("darkred", "darkgreen", "darkblue"),
    cat.cex = 1.85,
    cat.fontfamily = "serif",
    cat.dist = c(0.06, 0.06, 0.03),
    cat.pos = 0,
    ind =TRUE,
    scaled=TRUE,
    margin=0.05,
    euler.d=TRUE
  )
  
  grid.draw(vennplot)
  
}

global_dyn_ui<<-FALSE
global_dyn_ui_tab<<-FALSE
global_dyn_ui_img<<-FALSE
####################################################################################
############## Reactivity
####################################################################################

shinyServer(function(input, output, session) {  
  

  
  #volumes <- getVolumes() #c('R Installation'=R.home())
  #shinyDirChoose(input, 'selectdirectory', roots=volumes, session=session, restrictions=system.file(package='base'))  
  
  #print(volumes())
#   output$seldir <- renderUI({
#     if(FALSE)
#     {
#       print("macos")
#       res= actionButton("selectmacdirectory", label = "Select Directory")  
#     }
#     else 
#     {
#       print("windows")
#       res=shinyDirButton('selectdirectory', 'Select Directory', 'Please select a folder containing Squassh data')
#     }
#       res
#   })
#   

output$condnames <- renderText({
  #print('render cond names')
  if(is.null(dataInput()))
  {
    #print('touptut')  
    "No Squassh data selected"
  }
  else {
    paste(cond_names(), collapse="\n")
  }
})

outputOptions(output, "condnames", priority = 10)


values <- reactiveValues()

observe({
  input$selectall
  if(input$selectall!=0)
  {
    updateSelectInput(session, "condselect",
                      choices = cond_names(),
                      selected = cond_names()
                      
    )
    values$condselect =cond_names()
  }
  
},priority=2)


observe({
    if(input$feature_choice %in% c('Total object volume Venn diagram'))
    {
      isolate({
       updateSelectInput(session, "condselect",
                      choices = cond_names(),
                      selected = input$condselect[1]
                      
                        )
      values$condselect =input$condselect[1]
    })  
    }
  
},priority=2)



observe({
  input$deselectall
  if(input$deselectall!=0)
  {
    updateSelectInput(session, "condselect",
                      choices = cond_names(),
                      selected = vector('character')
                      
    )
    
    updateSelectInput(session, "repr",
                      choices = NULL,
                      selected = NULL
    )
    
    values$condselect=NULL
  }
  
},priority=2)


observe({
  input$applychanges
  
  isolate({
      values$condselect <- input$condselect        
  })
  
  #invalidateLater(1000, session) : for tunableresponsiveness
  
  
},priority=1)



  select_path <- reactive({
    selected_path<<- ""
    print(selected_path)
    #print(input$selectdirectory)
    #|| is.null(input$selectdirectory)
    #if(is.null(input$selectmacdirectory) ){
    #  #print("not loaded")
    #  selected_path
    #}
    #else{
    if(input$selectdirectory != 0)
       {
      #print("select mac dir")
      if(onmacos)
        selected_path <<- choose_dir_mac() 
      else
        selected_path <<- choose.dir() 
      
        if(is.na(selected_path))
          selected_path <<-"."
     }
    
    #if(length(parseDirPath(volumes, input$selectdirectory))!=0)
    #{
    #  print("select win/linux dir")
    #  selected_path <<- parseDirPath(volumes, input$selectdirectory)
    #}
    
    global_dyn_ui<<-FALSE
    global_dyn_ui_tab<<-FALSE
    global_dyn_ui_img<<-FALSE
    mov_comp_data <<- NULL
    mov_comp_name <<- NULL
    obschanoutput$resume()  
    obstaboutput$resume() 
    obsimgoutput$resume() 
    
    
    #print("select path")
    print(selected_path)
    selected_path
    #}
  })
  
  
output$path <-renderText({
  select_path()
})

  #data loading when pressing "select directory"
  dataInput <- reactive({
    #print('data input')
    data=NULL
    if(select_path()!="" && select_path()!=".")#input$selectmacdirectory!=0)#
    {
      data = load_data(select_path())  
    }
    data
  })

ismovie <- reactive({
  if(empty_data(dataInput()))
    FALSE
  else {
    #print('ismovieb')
    file1 = dataInput()[[1]][[4]][1,1]
    file2 = dataInput()[[1]][[4]][2,1]
    #print(file1)
    #print(file2)
    ismov = (file1 == file2)
    #print('ismov')
    #print(ismov)
    ismov
  }
})
  
  
  
  maxSizes<-reactive({
    c(input$maxsizeC1,input$maxsizeC2,input$maxsizeC3)
  })
  
  minSizes<-reactive({
    c(input$minsizeC1,input$minsizeC2,input$minsizeC3)
  })
  
  minIntensities<-reactive({
    c(input$minintC1,input$minintC2,input$minintC3)
  })
  
  minObjs<-reactive({
    c(input$minobjC1,input$minobjC2,input$minobjC3)
  })
  
  colocChannels <- reactive({
    as.integer(input$channel_choice_coloc2)
  })
  
  ChannelChoice <- reactive({
    feature= input$feature_choice
    
    p=as.integer(input$channel_choice_pearson)
    a=as.integer(input$channel_choice_all)
    coloc=as.integer(input$channel_choice_coloc)
    
    res=a
    
    #print("channle choice")
    if(feature %in% c('Colocalization (number)','Colocalization (size)','Colocalization (signal)'))
    {
      res=coloc
    }
    if(feature %in% c('Pearson correlation','Pearson correlation inside cell mask'))
    {
      #print('pearson ici')
      #print(input$channel_choice_pearson)
      res=p
      #print(res)
    }

    res
  })

  
  #selected conditions (reorder at the same time w.r.t to user input)
  selectData <- reactive({
    #print("select data")
    selected_conds=values$condselect
    #print(selected_conds)
    if(empty_data(selected_conds))
    {
      
      list()
    }
    else{
      
    conds=sapply(values$condselect,match,table=cond_names())
    
    #print(conds)
    dataInput()[conds]
    }
  })
  
  filteredData <- reactive({
    
    test = input$tabs
    
    #print("test tabs")
    #print(test)
    
    if(empty_data(selectData()) || test != "Analysis"){
     #print('no data') 
      NULL
    }
    else {
      #print("filter data")
      conds=sapply(values$condselect,match,table=cond_names())
      newdata=filter_data(selectData(),matrix(include_matrix[,conds], ncol=length(conds)), maxSizes(), minSizes(), minIntensities(), minObjs())
    }
  })
  

  
  
  ########################
  ## data characteristics
  nb_conditions <- reactive({
    if(empty_data(dataInput()))
      0
    else {
    nbconditions<<-length(dataInput())   
    nbconditions
    }
  })
  
  
  max_nb_imgs <- reactive({
    if(empty_data(dataInput()))
      0
    else {
    nb_imgs=NULL
    for(i in 1:length(dataInput())){
      nb_imgs=c(nb_imgs, nrow(dataInput()[[i]][[4]]) )
    }
    max(nb_imgs)
    }
  })
  
  
  max_channels <- reactive({
    if(empty_data(dataInput()))
      0
    else {
    max_channels = max(sapply(dataInput(),length)) -4  
    #print(paste("max channels", max_channels))
    max_chans<<-max_channels
    max_channels
    }
  })

  
  
  output$nbchannel<-reactive({max_channels()}) 

  
  
  dir_path <- reactive({
    if(is.null(dataInput()))
      "no data selected"
    else {
      selected_path
    }
  })
  
  cond_names <- reactive({
    if(empty_data(dataInput()))
      "No data found."
    else {
    data= dataInput()
    res=NULL
    for(i in 1:length(data)){
      res=c(res, data[[i]][[1]] )
    }
    res
    }
  })
    
  
  output$stats_disp <- renderText({
    if(empty_data(dataInput()))
      ""
    else {
      #input$stat_display
      #print("gfdgfdgdf")
    #print("call s")
    #isolate({string_res=stats()})
      string_res=stats()
     # print(string_res)

    paste(string_res, collapse="\n")
      
    }
  })
  

  ####################
  
  #create exclude list
  excl_list <- observe({
    list = NULL
    contact_sheet_sel=input$contact_sheet_condition
    cond=grep(contact_sheet_sel,cond_names(), fixed = TRUE)
    print(empty_data(dataInput()))
    print(contact_sheet_sel=="")
    print(length(cond)==0)
    print(length(cond_names())==0)
    
    if(empty_data(dataInput()) ||
         contact_sheet_sel=="" ||
         length(cond)==0 ||
         length(cond_names())==0)#|is.null(input$excl1_1)
    {
      
      print('excl list')
     
      list
      
    }
    else {
      cond=grep(contact_sheet_sel,cond_names(), fixed = TRUE)
      #print("excl list 1")
      #print(cond)
      nb = nrow(dataInput()[[cond]][[4]])
      vartest=paste0("excl",cond,"_",1)
      #print("excl list 2")
      #print(vartest)
      if(is.null(input[[vartest]])){
        print('vartest')
        list
      }
      else{
        for(i in 1:nb){
          excl=paste0("excl",cond,"_",i)
          if(input[[excl]])
          {list=c(list,i)
           #print("ici")
           include_matrix[i,cond]<<-FALSE}
          #print("ici2")
        }
        #print(include_matrix)
        #print("exclude")
        #print(cond)
      }
    }
  })
  
  
  output$nbconds <- renderText({
    if(is.null(dataInput())){
      ""
    }
    else
    {
    nb = nb_conditions()
    if(nb == 0)
      ""
    else {
      #print("matrix")
      #print(nb)
      #print(max_nb_imgs())
      include_matrix<<-matrix(nrow=max_nb_imgs(), ncol=nb,TRUE)
      if(ismovie())
        paste(nb_conditions() ,"live cell imaging","condition(s) found with ", max_channels(), "channels :")
      else
        paste(nb_conditions() ,"condition(s) found with ", max_channels(), "channels :")
    }
    }
  })

output$warningw <- renderText({
    "Please switch to R for directory selection window."
})



output$platform <- renderText({
  paste(onmacos)
})
  
  
output$movie <- renderText({
  #print(ismovie())
  paste(ismovie())
})
  
outputOptions(output, "movie", suspendWhenHidden = FALSE)
  




output$excluded_imgs <- renderText({
  selected_tab = input$tabs
  if(empty_data(dataInput()) || selected_tab!="Analysis" || all(include_matrix == TRUE))
    "None"
  else {
    
    
    nco=ncol(include_matrix)
    nro=nrow(include_matrix)
    final_str=NULL
    for(j in 1:nco){
      str_col=NULL  
      for(i in 1:nro){
        if(!include_matrix[i,j])  str_col=paste(str_col,i-1) 
      }
      final_str=paste(final_str, isolate({cond_names()[[j]]}),": ", str_col , "\n") 
    }
    final_str
    
  }
})

  
  output$images_caption <- renderText({
    contact_sheet_sel=input$contact_sheet_condition
    if(is.null(dataInput()) | contact_sheet_sel=="")
      ""
    else {
    paste(input$contact_sheet_condition, "images")
    }
  })
  
  
  img_features <- reactive({
   # print("compute features")
    #print(ChannelChoice())
    #print(values$condselect)
    test=input$feature_choice %in%  c('Colocalization (number)','Colocalization (size)','Colocalization (signal)')
    #print(input$channel_choice_coloc2)
    test2=is.null(input$channel_choice_coloc2)
    condnames = values$condselect
    test3= test && test2
    #print(test)
    #print(test2)
    #print(test3)
    if(
        is.null(filteredData()) ||
          (
            (input$feature_choice %in%  c('Colocalization (number)','Colocalization (size)','Colocalization (signal)'))
            && (is.null(input$channel_choice_coloc2)||input$channel_choice_coloc2==input$channel_choice_coloc)
            )
        ||
          (
            (input$feature_choice %in%  c('Pearson correlation','Pearson correlation inside cell mask'))
            && (length(input$channel_choice_pearson)<2) && max_channels()>2
          )
        ||
          (input$feature_choice %in% c('Total object volume Venn diagram'))
      )
      NULL
    else {
      max_imgs<<-max_nb_imgs() #get and store max nb of images
      #print("do calc")
      #print(input$feature_choice)
      #print(ChannelChoice())
      #print("coloc channel")
      #print(colocChannels())
      withProgress(message = 'Analyzing',{
      result=feature_computation(input$feature_choice,
                                 filteredData(),
                                 ChannelChoice(),
                                 colocChannels(),
                                 maxSizes(),
                                 minSizes(),
                                 minIntensities(),
                                 condnames)}
      )
      #print(condnames)
      #print(result)
      colnames(result)<-condnames
      result
    }
  })

compute_venn_2chans <- reactive({
  if(is.null(filteredData())
     ||
       !(input$feature_choice %in% c('Total object volume Venn diagram'))
     )
    NULL
  else{
  #take first condition only : data= filteredData()[[1]]
    #print('recompute venn data')
  data=filteredData()[1]
  
  TS1=total_ves_volume(data,FALSE,1)
  TS2=total_ves_volume(data,FALSE,2)
  TS12=total_ves_volume_multi(data,1,2,maxSizes(), minSizes(), minIntensities()) 
  
  TS1m = mean(TS1,na.rm = TRUE)
  TS2m = mean(TS2,na.rm = TRUE)
  TS12m = mean(TS12,na.rm = TRUE)
  
  c(TS1m,TS2m,TS12m)
  }
})


compute_venn_3chans <- reactive({
  if(is.null(filteredData())
     ||
       !(input$feature_choice %in% c('Total object volume Venn diagram'))
  )
    NULL
  else{
    #take first condition only : data= filteredData()[[1]]
    #print('recompute venn data')
    data=filteredData()[1]
    
    TS1=total_ves_volume(data,FALSE,1)
    TS2=total_ves_volume(data,FALSE,2)
    TS3=total_ves_volume(data,FALSE,3)
    
    
    TS12=total_ves_volume_multi(data,1,2,maxSizes(), minSizes(), minIntensities()) 
    TS23=total_ves_volume_multi(data,2,3,maxSizes(), minSizes(), minIntensities()) 
    TS13=total_ves_volume_multi(data,1,3,maxSizes(), minSizes(), minIntensities()) 
    TS123=total_ves_volume_multi(data,1,c(2,3),maxSizes(), minSizes(), minIntensities()) 
    
    
    TS1m = mean(TS1,na.rm = TRUE)
    TS2m = mean(TS2,na.rm = TRUE)
    TS3m = mean(TS3,na.rm = TRUE)
    
    TS12m = mean(TS12,na.rm = TRUE)
    TS23m = mean(TS23,na.rm = TRUE)
    TS13m = mean(TS13,na.rm = TRUE)
    
    TS123m = mean(TS123,na.rm = TRUE)
    
    #print('results')
    #print(TS1m)
    #print(TS2m)
    #print(TS3m)
    #print(TS12m)
    #print(TS23m)
    #print(TS13m)
    #print(TS123m)
    
    c(TS1m,TS2m,TS3m,TS12m,TS23m,TS13m,TS123m)
  }
})

  
  ylim_max <- reactive({
  max(img_features(),na.rm=TRUE)    
  
  })

  ylim_min <- reactive({
    min(0,min(img_features(),na.rm=TRUE))    
})


 stats <- reactive ({
   if(length(values$condselect)>1 && !(is.null(img_features())))
   stat_aov_tukey(img_features(),length(values$condselect), values$condselect)
   else
     "Statistical analysis is done when at least two conditions are selected."
 })

movie_compare <- observe({
  print('compare')
  input$compare_movie
  isolate({
    if(!is.null(img_features())) 
    {
      print("do compare")
    mov_comp_data <<- rollmean_data()
    }
    else{
      print("no compare")
      mov_comp_data <<- NULL
    }
      
  })
})


movie_compare_name <- observe({
  print('compare name')
  input$compare_movie
  isolate({
    if(!is.null(img_features())) 
    {
      print("do compare name")
      mov_comp_name <<- paste(input$feature_choice, chan_name())
      print(mov_comp_name)
    }
    else{
      print("no compare name")
      mov_comp_name <<- NULL
    }
    
  })
})


observe({
  input$compare_reset
  mov_comp_name <<- NULL
  mov_comp_data <<- NULL      
})




rollmean_data <- reactive({
  if(is.null(filteredData()) ||
       (is.null(img_features()) && !(input$feature_choice %in% c('Total object volume Venn diagram'))) 
  )
    0
  else {
    #print(input$rolltmean)
    #time_order= mixedorder(filteredData()[[1]][[4]]$File)
    #dat=img_features()[time_order]
    #reorder data according to time (name of files)
    input$compare_reset
    reordered_data=reorder_movie_data(img_features(),filteredData())
    #rolldat=rollmean(reordered_data,input$rolltmean, fill="extend") 
    rolldat=rollmean(img_features(),input$rolltmean, fill="extend") #deactivate reordering with new squassh version
    
    
    
    
    #print(rolldat)
    
    # rolldat=rollmean(dat,input$rolltmean, fill="extend")
    
    rolldat
  }
})


 chan_name <-reactive({
   chans=c(input$ch1name,input$ch2name,input$ch3name)
   coloc=input$feature_choice %in%  c('Colocalization (number)','Colocalization (size)','Colocalization (signal)') 
    pearson =input$feature_choice %in%  c('Pearson correlation','Pearson correlation inside cell mask')
    other = !coloc && !pearson

if(other)
  res=chans[ChannelChoice()]
if(coloc){
 print('coloc')
 print(chans[colocChannels()])
  res=paste(chans[ChannelChoice()],'with',paste(chans[colocChannels()], collapse=', '))
  
}
  if(pearson) {
    if(max_channels()==2)
      res=paste(chans[1],'and',chans[2])
    else
      res=paste(chans[ChannelChoice()],sep=' and ')
  }
    

res
 })


 output$boxplot <- renderPlot({
    if(is.null(filteredData()) ||
         (is.null(img_features()) && !(input$feature_choice %in% c('Total object volume Venn diagram'))) 
          )
      0
    else {
      chan_names=chan_name()
      if(ismovie()){
        
        plot_movie(cbind(rollmean_data(),mov_comp_data),c(paste(input$feature_choice,chan_names),mov_comp_name),0,max(cbind(rollmean_data(),mov_comp_data),na.rm=TRUE), paste(input$feature_choice,'\n',chan_names))
      }
      else if(input$feature_choice %in% c('Total object volume Venn diagram'))
      {
        
        if(max_channels()==2)
          plot_venn_2chans(compute_venn_2chans(),input$ch1name,input$ch2name)
        else if(max_channels()==3)
          plot_venn_3chans(compute_venn_3chans(),input$ch1name,input$ch2name,input$ch3name)
      }
      else{    
    plot_features(input$graph_choice,as.data.frame(img_features()[,dispOrder()]),values$condselect[dispOrder()], ylim_min(),ylim_max(),paste(input$feature_choice,'\n',chan_names))
      }
    }
  })



dispOrder <- reactive({
  
  if(!input$displaysort)
  {
    1:length(values$condselect)
  }
  else
    {
      means = apply(img_features(),2,mean,na.rm = TRUE)
      #print(means)
      res=sort(means,index.return=TRUE)$ix
      #print(res)
      res
  }
  
})
  

output$exportPlot <- downloadHandler(
  filename = function() {
    paste0("Squassh_analyst_plot",".pdf")
  },
  content = function(path) {
    isolate({pdf(path) #, width=8.2, height=11.3)
             chan_names=chan_name()
             if(ismovie()){
               plot_movie(cbind(rollmean_data(),mov_comp_data),c(paste(input$feature_choice,chan_names),mov_comp_name),0,1.2*max(cbind(rollmean_data(),mov_comp_data),na.rm=TRUE), paste(input$feature_choice,'\n',chan_names))
             }
             else if(input$feature_choice %in% c('Total object volume Venn diagram'))
             {
               
               if(max_channels()==2)
                 plot_venn_2chans(compute_venn_2chans(),input$ch1name,input$ch2name)
               else if(max_channels()==3)
                 plot_venn_3chans(compute_venn_3chans(),input$ch1name,input$ch2name,input$ch3name)
             }
             else{    
               plot_features(input$graph_choice,as.data.frame(img_features()[,dispOrder()]),values$condselect[dispOrder()], ylim_min(),ylim_max(),paste(input$feature_choice,'\n',chan_names))
             }
    dev.off()
    })
  }
)


output$exportcsv <- downloadHandler(
  
  filename = function() {
      "Squassh_analyst_data.csv"
  },
  
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    
    # Write to a file specified by the 'file' argument
    write.table(img_features(), file, sep = ',',
                row.names = FALSE)#or row.names=TRUE and col.names=NA
  }
)




  observe({
    updateSelectInput(session, "contact_sheet_condition",
                    choices = cond_names(),
                    selected = 1)
    
    updateSelectInput(session, "condselect",
                      choices = cond_names()
                      )
    
  })


 observe({
  
  
  if(is.null(values$condselect))
   {updateSelectInput(session, "repres",
                     choices = c(Choose ='','No conditions computed.')
   )
  }
  else
    updateSelectInput(session, "repres",
                      choices = c(Choose='',values$condselect)
    )
  
 })





observe({
  #print("update coloc2")
  maxc = max_channels()
  #print(maxc)
  coloc1=input$channel_choice_coloc
  #print(coloc1)
  if(maxc==2){
    
    if(coloc1==1){
      vecc=c(2)
      names(vecc)<-input$ch2name
      updateSelectInput(session, "channel_choice_coloc2",
                        choices = vecc,
                        selected=2
                        
      )
    }
    if(coloc1==2){
      vecc=c(1)
      names(vecc)<-input$ch1name
      updateSelectInput(session, "channel_choice_coloc2",
                        choices = vecc,
                        selected=1
                        #options = list(maxItems = 1)
                        
      )
    }
    
    
  }
  
  if(maxc==3){    
    if(coloc1==1){
      vecc=c(2,3)
      names(vecc)<-c(input$ch2name,input$ch3name)
      updateSelectInput(session, "channel_choice_coloc2",
                        choices = vecc
      )
    }
    
    if(coloc1==2){
      vecc=c(1,3)
      names(vecc)<-c(input$ch1name,input$ch3name)
      updateSelectInput(session, "channel_choice_coloc2",
                        choices = vecc
      )
    }
    
    if(coloc1==3){
      vecc=c(1,2)
      names(vecc)<-c(input$ch1name,input$ch2name)
      updateSelectInput(session, "channel_choice_coloc2",
                        choices = vecc
      )
    }
    
  }
    
  
},priority=10)

observe({
  
  #feature= input$feature_choice
  
  #print("update nb chans")
  maxc = max_channels()
  
  #print("nb channels")
  #print(maxc)
  if(maxc==1){
    vecc=c(1)
    names(vecc)<-c(input$ch1name)
    updateSelectInput(session, "channel_choice_all",
                      choices = vecc
                      
    )

    updateSelectInput(session,"feature_choice",
                      choices=
                      c("Mean object volume", 
                        "Mean object length", 
                        "Mean object intensity",
                        "Total object signal",
                        "Total object signal / Cell Size",
                        "Total object volume",
                        "Total object volume / Cell Size",
                        "Object number",
                        "Object number / Cell Size"
                      ), 
                        )
    
  }
  
  if(maxc==2){
    vecc=c(1,2)
    names(vecc)<-c(input$ch1name, input$ch2name)
    
    updateSelectInput(session, "channel_choice_pearson",
                      choices = vecc
                      
    )
    updateSelectInput(session, "channel_choice_all",
                      choices = vecc
                      
    )
    updateSelectInput(session, "channel_choice_coloc",
                      choices = vecc
                      
    )
    updateSelectInput(session,"feature_choice",
                      choices=
                        c("Mean object volume", 
                          "Mean object length", 
                          "Mean object intensity",
                          "Colocalization (number)", 
                          "Colocalization (size)", 
                          "Colocalization (signal)",
                          "Pearson correlation", 
                          "Pearson correlation inside cell mask", 
                          "Total object volume Venn diagram",
                          "Total object signal",
                          "Total object signal / Cell Size",
                          "Total object volume",
                          "Total object volume / Cell Size",
                          "Object number",
                          "Object number / Cell Size"
                          
                        ), 
    )
    
    
    
  }
    
    if(maxc==3){
      vecc=c(1,2,3)
      names(vecc)<-c(input$ch1name,input$ch2name,input$ch3name)
      updateSelectInput(session, "channel_choice_pearson",
                        choices = vecc
                        ,selected=c(1,2)
      )
      updateSelectInput(session, "channel_choice_all",
                        choices = vecc
      )
      updateSelectInput(session, "channel_choice_coloc",
                        choices = vecc
      )
      updateSelectInput(session,"feature_choice",
                        choices=
                          c("Mean object volume", 
                            "Mean object length", 
                            "Mean object intensity",
                            "Colocalization (number)", 
                            "Colocalization (size)", 
                            "Colocalization (signal)",
                            "Pearson correlation", 
                            "Pearson correlation inside cell mask", 
                            "Total object volume Venn diagram",
                            "Total object signal",
                            "Total object signal / Cell Size",
                            "Total object volume",
                            "Total object volume / Cell Size",
                            "Object number",
                            "Object number / Cell Size"
                          ), 
      )
      
  }
  
  
  #print(feature)
  #if(feature %in% c('ColocalizationNumber','ColocalizationSize','ColocalizationSignal'))
  #{
   # print("selctize coloc")
  #  updateSelectInput(session, "channel_choice",
   #                 label = "Colocalization of channel ...",
    #                choices = list("Channel 1" = 1, "Channel 2" = 2, "Channel 3" = 3)
     #                 )

  #}
  
  
})
  
output$condch1 <- renderUI({
    res= column(4,offset=0,
                h4(input$ch1name),
                #condition = "output.max_channels==2",
                sliderInput("minsizeC1", "Minimum object size [pixel^3]:", 
                            min=0, max=50, value=0),
                sliderInput("maxsizeC1", "Maximum object size [pixel^3]:", 
                            min=100, max=10000, value=10000),
                sliderInput("minintC1", "Mininmum intensity [0,1]:", 
                            min=0, max=1, value=0, step=0.01),
                sliderInput("minobjC1", "Mininimum #objects per image:", 
                            min=0, max=20, value=0)
    )
                
    
  res
})



output$condch2 <- renderUI({
  if(max_channels()>1)
  res= column(4,offset=0,
         h4(input$ch2name),
         #condition = "output.max_channels==2",
         sliderInput("minsizeC2", "Minimum object size [pixel^3]:", 
                     min=0, max=50, value=0),
         sliderInput("maxsizeC2", "Maximum object size [pixel^3]:", 
                     min=100, max=10000, value=10000),
         sliderInput("minintC2", "Mininmum intensity [0,1]:", 
                     min=0, max=1, value=0, step=0.01),
         sliderInput("minobjC2", "Mininimum #objects per image:", 
                     min=0, max=20, value=0)
         
  )
  else res=NULL
  res
})

output$condch3 <- renderUI({
  if(max_channels()>2)
    res= column(4,offset=0,
                       h4(h5(input$ch3name)),
                       sliderInput("minsizeC3", "Minimum object size [pixel^3]:", 
                                   min=0, max=50, value=0),
                       sliderInput("maxsizeC3", "Maximum object size [pixel^3]:", 
                                   min=100, max=10000, value=10000),
                       sliderInput("minintC3", "Mininmum intensity [0,1]:", 
                                   min=0, max=1, value=0, step=0.01),
                       sliderInput("minobjC3", "Mininimum #objects per image:", 
                                   min=0, max=20, value=0)
                
    )
  else res=NULL
  res
})

output$optional <- renderUI({
  if(max_channels()>0)
    res= h4('Optional :')
  else res=NULL
  res
})


output$ch1name <- renderUI({
  if(max_channels()>0)
    res= textInput('ch1name', h5('Rename channel 1'), value = "Channel 1")
  else res=NULL
  res
})

output$ch2name <- renderUI({
  if(max_channels()>1)
    res= textInput('ch2name', h5('Rename channel 2'), value = "Channel 2")
  else res=NULL
  res
})

output$ch3name <- renderUI({
  if(max_channels()>2)
    res= textInput('ch3name', h5('Rename channel 3'), value = "Channel 3")
  else res=NULL
  res
})



output$ch1 <- renderText({
  'Channel 1'
})

output$ch2 <- renderText({
  'Channel 2'
})

output$ch3 <- renderText({
  'Channel 3'
})


output$seg_params <- renderText({
  contact_sheet_sel=input$contact_sheet_condition
  if(is.null(dataInput()) | contact_sheet_sel=="")
    ""
  else { 
    cond=grep(contact_sheet_sel,cond_names(), fixed = TRUE)
    dataInput()[[cond]][[3]]
  }
})

  output$ui <- renderUI({
    contact_sheet_sel=input$contact_sheet_condition
    if(is.null(dataInput()) | contact_sheet_sel=="")
      tags$h4("No data selected")
    else {
    
    cond=grep(contact_sheet_sel,cond_names(), fixed = TRUE)
    
    path = dataInput()[[cond]][[2]]
    nb = nrow(dataInput()[[cond]][[4]])
    #print('renderUI')
    c_list = c(list(p("")))
    for(i  in nb:1){
      #print('render')
      excl=paste0("excl",cond,"_",i)
      #print(excl)
      img=paste0("img",i)
      segr=paste0("seg",i)
      namer=paste0("name",i)
      ch1l=paste0("ch1l",i)
      ch2l=paste0("ch2l",i)
      ch3l=paste0("ch3l",i)
      c_list  =  c(list(wellPanel(tags$h4(paste0("Image ",(i-1),", original/segmentation with cell mask")),
                                  textOutput(namer),
                                  textOutput(ch1l,inline=TRUE),
                                  textOutput(ch2l,inline=TRUE),
                                  textOutput(ch3l,inline=TRUE),  
                                  fluidRow(
                                      column(8,imageOutput(img,width = 600,height = 300)),
                                      column(4,p("Mean image features"),
                                             tableOutput(segr))
                                            ),
                                  tags$style(type='text/css', paste0('#',ch1l,' {color: red;}')),
                                  tags$style(type='text/css', paste0('#',ch2l,' {color: green;}')),
                                  tags$style(type='text/css', paste0('#',ch3l,' {color: blue;}')),
                                  #tags$style(type='text/css', "#exportPlot {margin-top: 20px;}"),
                                  #tags$style(type='text/css', "#exportPlot {margin-top: 20px;}"),
                                  
                                    checkboxInput(excl, tags$h5("Exclude image"), FALSE)
                                  )
                        
                        )
                   ,c_list) 
    }
    tagList(
      c_list
    )
  }
  })
  


#p(
#   span("(Channel 1)", style = "color:red"),
#  span("(Channel 2)", style = "color:green"),
#   span("(Channel 3)", style = "color:blue")
# )
# 
#   for(i in 1:30){
#   imgname= paste0("img",i)
#   expr_plot_img=substitute({
#     contact_sheet_sel=input$contact_sheet_condition
#     if(is.null(dataInput()) | contact_sheet_sel=="")
#     {}
#     else{
#       cond=grep(contact_sheet_sel,cond_names())
#       path = dataInput()[[cond]][[2]]
#       
#       outfile=paste0(path,(ii-1),"_cs.png")
#       # Return a list containing the filename
#       list(src = outfile,
#            contentType = 'image/png',
#            width = 600,
#           # height = 384,
#            alt = "contact sheet image not found")
#     }
#     
#     
#   },list(ii = i))
# 
#   
#   output[[imgname]] <- renderImage(expr_plot_img, quoted= TRUE , deleteFile = FALSE)
#   }

observe({
  #print('suspend')
  contact_sheet_sel=input$contact_sheet_condition
  if(global_dyn_ui && global_dyn_ui_tab && global_dyn_ui_img){
    obschanoutput$suspend()  
    obstaboutput$suspend() 
    obsimgoutput$suspend() 
    #print('suspend done')
  }
},priority=1)


obstaboutput <- observe({

  contact_sheet_sel=input$contact_sheet_condition
  isolate({
    if(is.null(dataInput()) | contact_sheet_sel=="")
    {print('no tab update')}
    else {
      global_dyn_ui_tab<<-TRUE
      #print('observe channels tabs')
      nbi = max_nb_imgs()
      #print(nbi)
      
      for(i in 1:nbi){
        segr=paste0("seg",i)
        namer=paste0("name",i)
        #print(segr)
        #print(namer)
        
        expr_segr_img=substitute({
          contact_sheet_sel=input$contact_sheet_condition
          if(is.null(dataInput()) | contact_sheet_sel=="")
          {}
          else{
            cond=grep(contact_sheet_sel,cond_names(),fixed = TRUE)
            dataf = dataInput()[[cond]][[4]]
            res= dataf[dataf$Image.ID==(ii-1),]
            rownames(res)<-paste("Img",(ii-1))
            # Return a string containing the image analysis   
            if(max_channels()==1){
              res=t(res[c("Objects.ch1")])
              
              rnames=c('Objects in ch1')
              rownames(res)<-rnames
            }
            
            
            if(max_channels()==2){
              res=t(res[c("Objects.ch1",
                          "Objects.ch2",
                          "Colocalization.ch1.in.ch2..size.based.",
                          "Colocalization.ch2.in.ch1..size.based.",
                          "Pearson.correlation",
                          "Pearson.correlation.inside.cell.masks")
                        ])
              rnames=c('Objects in ch1',
                       'Objects in ch2',
                       'Coloc ch1 in ch2 (Size)',
                       'Coloc ch2 in ch1 (Size)',
                       'Pearson',
                       'Pearson in cell masks'
              )
              rownames(res)<-rnames
              
            }
            
            if(max_channels()==3){
              res=t(res[c("Objects.ch1",
                          "Objects.ch2",
                          "Objects.ch3",
                          "Total.size.ch12",
                          "Total.size.ch23",
                          "Total.size.ch13"
              )
              ])
              rnames=c('Objects in ch1',
                       'Objects in ch2',
                       'Objects in ch3',
                       'Total size ch1^ch2',
                       'Total size ch2^ch3',
                       'Total size ch1^ch3'               
              )
              rownames(res)<-rnames
            }
            
            res
          }
          
          
        },list(ii = i))
        expr_namer_img=substitute({
          contact_sheet_sel=input$contact_sheet_condition
          if(is.null(dataInput()) | contact_sheet_sel=="")
          {}
          else{
            cond=grep(contact_sheet_sel,cond_names(),fixed = TRUE) 
            dataf = dataInput()[[cond]][[4]]
            res= factor(dataf[dataf$Image.ID==(ii-1),'File'])
            res=levels(res)
            res
          }
          },list(ii = i))
        
        output[[segr]] <- renderTable(expr_segr_img, quoted= TRUE)
        output[[namer]] <- renderText(expr_namer_img, quoted= TRUE)
        
        
        
        
          
          
        
        
           }#for
            }#else
         })#isolate

},priority=3)




obsimgoutput <- observe({
  
  contact_sheet_sel=input$contact_sheet_condition
  isolate({
    if(is.null(dataInput()) | contact_sheet_sel=="")
    {}
    else {
      global_dyn_ui_img<<-TRUE
      #print('observe channels tabs')
      nbi = max_nb_imgs()
      #print(nbi)
      
      for(i in 1:nbi){
        imgname= paste0("img",i)
        expr_plot_img=substitute({
          contact_sheet_sel=input$contact_sheet_condition
          if(is.null(dataInput()) | contact_sheet_sel=="")
          {
            
            list(src = '',
                 contentType = 'image/png',
                 width = 600,
                 # height = 384,
                 alt = "contact sheet image")
            
            
          }
          else{
            cond=grep(contact_sheet_sel,cond_names(),fixed = TRUE)
            path = dataInput()[[cond]][[2]]
            print("working directory : ")
            print(getwd())
            #print(list.files(path='.'))
            
            outfile=paste0(path,(ii-1),"_cs.png")
            #print(file.path(getwd(),outfile))
            print(path)
            print(outfile)
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 width = 600,
                 height = 300,
                 alt = "contact sheet image not found")
          }
          
          
        },list(ii = i))
        
        
        output[[imgname]] <- renderImage(expr_plot_img, quoted= TRUE , deleteFile = FALSE)
        
      }#for
    }#else
  })#isolate
  
},priority=3)

# for(i in 1:2){
#   segr=paste0("seg",i)
#   expr_segr_img=substitute({
#     contact_sheet_sel=input$contact_sheet_condition
#     if(is.null(dataInput()) | contact_sheet_sel=="")
#     {}
#     else{
#       cond=grep(contact_sheet_sel,cond_names())
#       dataf = dataInput()[[cond]][[4]]
#       res= dataf[dataf$Image.ID==(ii-1),]
#       rownames(res)<-paste("Img",(ii-1))
#       # Return a string containing the image analysis   
#       if(max_channels()==1){
#         res=t(res[c("Objects.ch1")])
#         
#         rnames=c('Objects in ch1')
#         rownames(res)<-rnames
#       }
#       
#       
#       if(max_channels()==2){
#       res=t(res[c("Objects.ch1",
#             "Objects.ch2",
#             "Colocalization.ch1.in.ch2..size.based.",
#             "Colocalization.ch2.in.ch1..size.based.",
#             "Pearson.correlation",
#             "Pearson.correlation.inside.cell.masks")
#             ])
#       rnames=c('Objects in ch1',
#                'Objects in ch2',
#                'Coloc ch1 in ch2 (Size)',
#                'Coloc ch2 in ch1 (Size)',
#                'Pearson',
#                'Pearson in cell masks'
#                )
#       rownames(res)<-rnames
#       
#       }
#       
#       if(max_channels()==3){
#         res=t(res[c("Objects.ch1",
#                     "Objects.ch2",
#                     "Objects.ch3",
#                     "Total.size.ch12",
#                     "Total.size.ch23",
#                     "Total.size.ch13"
#                     )
#                   ])
#         rnames=c('Objects in ch1',
#                  'Objects in ch2',
#                  'Objects in ch3',
#                  'Total size ch1^ch2',
#                  'Total size ch2^ch3',
#                  'Total size ch1^ch3'               
#         )
#         rownames(res)<-rnames
#       }
#       
#       res
#     }
#     
#     
#   },list(ii = i))
#   
#   
#   output[[segr]] <- renderTable(expr_segr_img, quoted= TRUE)
# }




obschanoutput <- observe({
  contact_sheet_sel=input$contact_sheet_condition
  
  isolate({
  if(is.null(dataInput()) | contact_sheet_sel=="")
    {}
  else {
    global_dyn_ui<<-TRUE
    #print('observe channels')
    nbi = max_nb_imgs()
    #print(nbi)

for(i in 1:nbi){
  #print('render channels')
  ch1l=paste0("ch1l",i)
  ch2l=paste0("ch2l",i)
  ch3l=paste0("ch3l",i)
  
  output[[ch1l]] <- renderText({
    input$ch1name
  })
  output[[ch2l]] <- renderText({
    if(max_channels()>1)
      input$ch2name
    else
      ""
  })
  output[[ch3l]] <- renderText({
    if(max_channels()>2)
      input$ch3name
    else
      ""
  })
  
  }
  }
  })
}, priority = 3)



output$ch1rep <- renderText({
  input$ch1name
})
output$ch2rep <- renderText({
  if(max_channels()>1)
    input$ch2name
  else
    ""
})
output$ch3rep <- renderText({
  if(max_channels()>2)
    input$ch3name
  else
    ""
})


output$namerep <- renderText({
  condrep = input$repres
  if(repimg()!=-1){
    cond_data=grep(condrep,cond_names(),fixed = TRUE)
  dataf = dataInput()[[cond_data]][[4]]
  res= factor(dataf[dataf$Image.ID==repimg()-1,'File'])
  res=levels(res)
  #print(res)
  res
  }
})



output$attrrep <- renderText({

  paste('Image whose \'', input$feature_choice,'\' value is closest to the median of the group is :', sep='')

})


output$valuerep <- renderText({
  condrep = input$repres
  if(repimg()!=-1){
    comp_data = img_features()
    medians = apply(comp_data,2,median,na.rm = TRUE)
    cond_comp=grep(condrep,values$condselect,fixed = TRUE)
    paste('Condition median value :',
          signif(medians[cond_comp],3),
          ', image value :',
          signif(comp_data[repimg(),cond_comp],3))  
  }
  else{
    ''
  }
  
})






output$imgrep =renderImage({
  condrep = input$repres
  
        if(repimg()!=-1){
  
          
        cond_data=grep(condrep,cond_names(),fixed = TRUE)
        path = dataInput()[[cond_data]][[2]]
    
        #print(path)
        outfile=paste0(path,repimg()-1,"_cs.png")
        #print(outfile)
                #       
                # Return a list containing the filename
          
        list(src = outfile,
           contentType = 'image/png',
           width = 512,
          height = 256,
           alt = "contact sheet image not found")
          }
        else
        {
          list(src = '',
               contentType = 'image/png',
               width = 512,
              height = 256,
               alt = "represenatative image")
          
        }
        }, deleteFile = FALSE)


repimg <- reactive({
  condrep = input$repres
  
  
    if(is.null(dataInput()) || condrep=="" || condrep=="No conditions computed.")
    {
      #print('repimg')
      -1
    }
    else {
        #find representative image
        cond_comp=grep(condrep,values$condselect,fixed = TRUE)
  
        comp_data = img_features()
        medians = apply(comp_data,2,median,na.rm = TRUE)
        
        rep=which.min(abs(comp_data[,cond_comp] - medians[cond_comp]))
      
      rep
       
    }
  
})

  
  
})