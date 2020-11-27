# This (admittedly ugly) function takes a dataframe and returns parameters
# estimates for each level of the id variable and the given
# target and response variables.

# If there are non-target variables then nt.vars should be either a list column
# names or a character vectorsi.e. nt.vars = c("nt1", "nt2")

#=============

# Edits:  29/10/2020

# also returns dataframe with trialswise estimates per participant (Wt, Wn, Wg)

#=============


JV10_df <-
  function(d,
           id.var = "id",
           cond.var = NULL,
           tar.var = "target",
           res.var = "response",
           nt.vars = NULL) {
    id <- d[, id.var]
    
    l <- split(d, id)
    
    paras <-
      data.frame(
        id = FALSE,
        K = FALSE,
        Pt = FALSE,
        Pn = FALSE,
        Pu = FALSE
      )
    trial_paras <- NULL
    
    for (i in seq_along(l)) {
      df <- as.data.frame.list(l[i], col.names = colnames(l[i]))
      
      X <- as.matrix(df[, res.var])
      Tg <- as.matrix(df[tar.var])
      
      if (is.null(nt.vars)) {
        B <- JV10_fit(as.vector(X), as.vector(Tg), return.llW = TRUE)
      } else {
        NT = as.matrix(df[, nt.vars])
        B <-
          JV10_fit(as.vector(X), as.vector(Tg), as.matrix(NT), return.llW = TRUE)
      }
      id <- as.character(df[1, id.var])
      paras[i, 1] <- id
      paras[i, 2:5] <- B$B
      
      
      temp_W <- B$W
      temp_W$id <- id
      
      cond_list = NULL
      for (m in 1:length(cond.var)) {
        cond_list[m] <- get("df")[cond.var[m]]
      }
      
      for (j in 1:length(cond.var)) {
        temp_W[, cond.var[j]] <- character(nrow(temp_W))
        temp_W[, ncol(temp_W)] <- cond_list[j]
      }
      
      
      trial_paras <- rbind(trial_paras, temp_W)
      
    }
    return(list(paras = paras, trial_paras = trial_paras))
  }
