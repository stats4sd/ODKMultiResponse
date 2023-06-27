library(tidyverse)

multi_response_odk_single<-function(data,labelset,id_string=NULL,multicolumn=NULL,other_string=NULL,retain_empty=TRUE,language="English"){
  
  if(!paste0("label::",language)%in%colnames(labelset)){
    
    newlanguage<-gsub("label::","",colnames(select(labelset,contains("label")))[1])
    
    warning(paste("language",language,"not found. Using first column labelled",newlanguage,"instead"))
    language<-newlanguage
  }
  
  if((is.null(id_string) & is.null(multicolumn))|(!is.null(id_string) & !is.null(multicolumn))){
    stop("Must include input for id_string or multicolumn")
  }
  
  if(is.null(multicolumn)){
    data %>% 
      mutate(group=1) %>%
      select(X_uuid,group,contains(id_string)) %>%
      pivot_longer(contains(id_string),names_to = "value",values_to = "exist") %>%
      filter(exist==1) %>%
      mutate(value=str_remove(value,id_string)) %>%
      select(-exist) %>%
      full_join(
        labelset,by=c("value"="name"))->long_responses
  }
  else{
    
    data %>% 
      separate(multicolumn,sep=" ",into=paste0(multicolumn,1:(1+max(str_count(.[,multicolumn]," "))))) %>%
      select(X_uuid,contains(multicolumn)) %>%
      pivot_longer(contains(multicolumn),names_to = "selection",values_to = "value",values_drop_na = TRUE) %>%
      select(-selection) %>%
      mutate(group=1) %>%
      full_join(
        labelset,by=c("value"="name"))->long_responses
  }
  
  
  overall_respondents_bygroup <- data  %>% 
    mutate(group=1) %>%
    group_by(group) %>%
    summarise(N_respondents=n()) 
  
  long_responses %>%
    mutate(count=ifelse(is.na(group),0,1)) %>%
    mutate(group=1) %>%
    group_by(group) %>%
    mutate(N_responses=sum(count)) %>%
    ungroup() %>%
    select(value,group,label=contains(paste0("label::",language)),count,N_responses) %>%
    group_by(value,group,label,N_responses) %>%
    summarise(n=sum(count)) %>%
    ungroup() %>%
    full_join(overall_respondents_bygroup,by="group") %>%
    mutate(per_responses=n/N_responses,
           per_respondents=n/N_respondents) %>%
    ungroup() %>%
    select(value,label,n,per_responses,per_respondents,N_responses,N_respondents) %>%
    arrange(desc(per_responses))->output
  
  if(retain_empty==FALSE){
    output<-filter(output,per_responses>0)
  }
  
  
  
  return(output)
}

multi_response_odk<-function(data,labelset,id_string=NULL,multicolumn=NULL,group=NULL,other_string=NULL,retain_empty=TRUE,language="English"){
  
  if(!paste0("label::",language)%in%colnames(labelset)){
    
    newlanguage<-gsub("label::","",colnames(select(labelset,contains("label")))[1])
    warning(paste("language",language,"not found. Using first column labelled",newlanguage,"instead"))
  }
  
  if(!is.null(group)){
    
    output<-   data %>%
      split(data[,group],) %>%
      map(.f=multi_response_odk_single,labelset,id_string,multicolumn,other_string,retain_empty,language) %>%
      list_rbind(names_to = group) %>%
      suppressMessages() %>% suppressWarnings()
    
    
  }
  else{
    
    output<- multi_response_odk_single(data,labelset,id_string,multicolumn,other_string,retain_empty,language) %>%
      suppressMessages() %>% suppressWarnings()
    
  }
  return(output)
}

