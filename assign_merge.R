assign_merge <- function(endtib,sttib,col_end_name="",col_st_name="",log_vec=c(T),col_end_index=NULL,col_st_index=NULL){
  newtib <- left_join(endtib,sttib,by="id")
  if(col_end_name!=""){
    col_end_index <- match(col_end_name,names(newtib))
  }
  if(col_st_name!=""){
    col_st_index <- match(col_st_name,names(newtib))
  }
  newtib[log_vec,col_end_index] <- newtib[log_vec,col_st_index]
  id_ind <- match("id",names(newtib))
  newtib <- newtib[,c(id_ind,col_end_index)]
  names(newtib)[2] <- "x"
  newtib$x <- car::recode(newtib$x,"NA=999")
  names(newtib)[2] <- names(endtib)[col_end_index]
  newtib
}


assign_merge <- function(endtib,sttib,log_vec=c(T),col_end_index=NULL,col_st_index=NULL){
  newtib <- full_join(endtib,sttib,by="id")
  row.names(newtib) <- newtib$id
  newtib[log_vec,col_end_index] <- newtib[log_vec,col_st_index]
  id_ind <- match("id",names(newtib))
  newtib <- newtib[,c(id_ind,col_end_index)]
  names(newtib)[2] <- "x"
  newtib$x <- car::recode(newtib$x,"NA=999")
  names(newtib)[2] <- names(endtib)[col_end_index]
  newtib
}