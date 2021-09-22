
utilGrouping <- function(df){
  
  groups <- as.vector(matrix(0,nrow=nrow(df)))
  
  x <- c(1:nrow(df))
  
  count <- 1
  
  for (val in x) {
    if(df$Transaction_Status[val] == 'Approved' | (df$Transaction_Status[val]== 'Declined' & df$Account_ID[val] != df$Account_ID[val+1]))  {
      groups[val] = count
      count = count+1
    }
    else{
      groups[val] = count
    }
  }
  
  df <- dplyr::bind_cols(df, data.frame(groups)) 
  
  return(df)  
}

factorChange <- function(x) {
  
  x %>% equals(lag(x, default = x[1])) %>% not %>% as.numeric

  }


aggregateTest <- function(x){
  
  temp <- x %>% slice(rep(1:n(), each = 30)) %>% 
    mutate(Transaction_Timestamp = as.Date(Transaction_Timestamp))
  
  for (val in c(0 : (nrow(x)-1))) {
    
    temp$Transaction_Timestamp[c((val*30)+1: 30*(val+1))] <- seq(as.Date(x$Transaction_Timestamp[val+1]),length=30, by = '1 day')
  
    }
  
}

isDynamic <- function(x, var){
  
  x %>%   # Factor_A dynamic both Account_ID (6687 IDs) and groups (106 groups)
    group_by(groups,paste(var)) %>% 
    summarise(total=n()) %>% 
    ungroup() %>% 
    group_by(groups) %>% 
    filter(n()>1) %>% 
    distinct(groups) %>% 
    nrow()
  
}

create_rfplot <- function(rf, type){
  
  imp <- importance(rf, type = type, scale = F)
  
  featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[,1])
  
  p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.65) +
    coord_flip() + 
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, color = "black"),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = 15, color = "black"),
          axis.text.y  = element_text(size = 15, color = "black")) 
  return(p)
}
