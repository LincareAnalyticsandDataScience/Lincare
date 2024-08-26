#' Get_New_SiteID
#'
#' This function updates an old Site ID or CRDB to the current Site ID/CRDB. The Fields names must remain unchanged and the CRDB field will need to be added to both HIER and OLD objects if CRDB = TRUE.
#'
#' @param x Site ID or CRDB that you would like updated
#' @param CRDB TRUE = CRDB; FALSE = SiteID
#'
#' @return A Vector containing the updated Site ID/CRDB
#'
#' @examples Get_New_SiteID(x = "0000340", CRDB = FALSE)
#' [1] "08"
#'
#' @export
Get_New_SiteID <- function(x, CRDB){

  if(!exists("HIER")) stop("Please load in the Current Hierarchy Table.")
  if(!exists("OLD")) stop("Please load in the Archived Hierarchy Table.")


  if(CRDB){
    if (x %in% HIER$CRDB) {
      return(x)
      next
    }else{

      temp <- OLD[CRDB == x & NEW_CRDB != x]
      if(nrow(temp) == 0){

        return(x)
        next
      }
      temp <- temp[which.max(temp$EFFDTE)]
      new <- temp$NEW_CRDB

      while(!new %in% HIER$CRDB){
        new_temp <- OLD[CRDB == new]
        new_temp <- new_temp[which.max(new_temp$EFFDTE)]
        new <- new_temp$NEW_CRDB
      }
      return(new)
    }

  }else{
    if (x %in% HIER$`Site ID`) {
      return(x)
      next
    }else{

      temp <- OLD[LUCCID == x & NEWSITE != x]
      if(nrow(temp) == 0){

        return(x)
        next
      }
      temp <- temp[which.max(temp$EFFDTE)]
      new <- temp$NEWSITE

      while(!new %in% HIER$`Site ID`){
        new_temp <- OLD[LUCCID == new]
        new_temp <- new_temp[which.max(new_temp$EFFDTE)]
        new <- new_temp$NEWSITE
      }
      return(new)
    }
  }
}
