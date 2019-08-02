# load libraries
library(devtools)
library(writexl)
library(data.table)


people_placer <- function(production_A_title, production_B_title, production_C_title, googleform) {
  # ***** COMMONLY USED VARIABLES - entered by user in app *****
  
  # replace spaces with underscores
  production_A_title_u <- gsub(" ", "_", production_A_title)
  production_B_title_u <- gsub(" ", "_", production_B_title)
  production_C_title_u <- gsub(" ", "_", production_C_title)
  
  production_titles_u <- c(production_A_title_u, production_B_title_u, production_C_title_u)
  
  
  
  # ** ACTUAL GOOGLE FORMS DATA: CREATING MEMBERS DF **
  path <- paste0("../",googleform[1])
  members = read.csv(path, stringsAsFactors = FALSE)
  # delete timestamped row
  members <- members[,-1]
  # define column names
  column_names <- c("email", "name", "years_in_lux", "years_at_uw", "writer/director?",
                    "writer/director film", "pref_prod_1", "pref_prod_2", "pref_prod_3",
                    "pref_role_1", "pref_role_2", "pref_role_3", "importance", "notes")
  # rename column names so the program can reference them
  colnames(members) <- column_names
  
  
  
  # ** CREATING PRODUCTION DF **
  # list of roles, ordered by experience required
  roles <- c("Director", "Producer", "Special Effects", "Editor", "Director of Photography", "Assistant Director",
             "Soundtrack", "Camera Operator", "Assistant Camera Operator", "Sound", "Makeup", "Art Department", "Costume",
             "Script Supervisor")
  
  # create 1 empty production dataframe
  production <- data.frame(roles)
  # add roles row names
  row.names(production) <- roles
  # delete inital row of role names
  production$roles <- NULL
  
  # create column titles corresponding to each production
  prod_1_column_names <- sapply(column_names, function(x) paste0(production_titles_u[1], "_", x))
  prod_2_column_names <- sapply(column_names, function(x) paste0(production_titles_u[2], "_", x))
  prod_3_column_names <- sapply(column_names, function(x) paste0(production_titles_u[3], "_", x))
  # combine all column titles into one giant list of column titles
  all_column_names <- c(prod_1_column_names, prod_2_column_names, prod_3_column_names)
  
  # create 1 big "production" dataframe, name the columns and make every cell "NA"
  production[,all_column_names] <- NA
  
  
  # ** CREATING PA DF **
  # create a "pa" dataframe with 1 cell
  pa <- data.frame("pa")
  # delete inital row
  pa$X.pa. <- NULL
  
  # create 1 "pa" dataframe, name the columns and make every cell "NA"
  pa[,column_names] <- NA
  
  
  
  
  # ** SORTING PEOPLE **
  
  # sort members df by experience, so more experienced people get placed first
  members <- members[order(-members$years_in_lux, -members$years_at_uw),]
  
  
  # ** ALGORITHM **
  
  # define 1st production choice, 1st role choice, and if a member was just placed
  production_choice <- 1
  role_choice <- 1
  member_just_placed <- FALSE
  
  # run this loop while there are still members to sort
  while (!is.na(members[1,1])) {
    
    # reset to 1st production choice, 1st role choice, and if a member was just placed
    production_choice <- 1
    role_choice <- 1
    member_just_placed <- FALSE
    
    repeat {
      
      # ********* WRITERS/DIRECTORS *********
      if (members[1, "writer/director?"] == "Yes") {
        # paste together the correct column name to check in the "production" DF
        column_to_check <- paste0(members[1, 6], "_email")
        column_to_check <- gsub(" ", "_", column_to_check)
        # store the column # matching the column_to_check string for future use
        starting_column_number <- which(colnames(production) == column_to_check)
        
        # place member on production df
        for (i in 1:14) {
          production["Director", starting_column_number - 1 + i] <- members[1, i]
          # "production" df is now updated with the new placement!
          # delete the top row off of the "members" df 
        }
        members <- members[-1,]
        member_just_placed <- TRUE
        break
      }
      # **************************************
      
      
      
      
      # ********* SETTING UP VARIABLES *********
      # store the role to check
      role_to_check <- members[1, paste0("pref_role_", role_choice)]
      # store the row # matching the role_to_check string for future use
      starting_row_number <- which(rownames(production) == role_to_check)
      # paste together the correct column name to check in the "production" DF
      
      column_to_check <- paste0(members[1, production_choice + 6], "_email")
      column_to_check <- gsub(" ", "_", column_to_check)
      # store the column # matching the column_to_check string for future use
      starting_column_number <- which(colnames(production) == column_to_check)
      # **************************************  
      
      
      # ********* IMPORTANCE = ROLE *********
      if (members[1, "importance"] == "Role") {
        
        # if the current "role to check" is a pa, copy & paste the member onto the PA df,
        # remove the top row of the members df, and begin placing the next member.
        if (role_to_check == "Production Assistant") {
          
          for (i in 1:14) {
            # copy name into PA df
            pa[1, i] <- members[1, i]
          }
          
          # add blank row to top of pa df
          x <- rep(NA, ncol(pa))
          pa <- rbind(x, pa)
          
          # delete the top row off of the "members" df 
          members <- members[-1,]
          member_just_placed <- TRUE
          break
        }
        
        if (is.na(production[role_to_check, column_to_check])) {   # if the cell is empty in the "production" df,
          
          # THEN place member's name & info into the "production" df.
          for (i in 1:14) {
            production[starting_row_number, starting_column_number - 1 + i] <- members[1, i]
          }
          
          # "production" df is now updated with the new placement!
          # delete the top row off of the "members" df 
          members <- members[-1,]
          member_just_placed <- TRUE
          
        } else {    # however, if the row is NOT empty in the prod_df,
          
          # increment production_choice and role_choice according to their preference (role more important)
          production_choice <- production_choice + 1
          
          if (production_choice == 4) {
            
            production_choice <- 1
            role_choice <- role_choice + 1
            
            # if all choices are exhausted, add this member to the PA df
            if (role_choice == 4 && production_choice == 1) {
              
              for (i in 1:14) {
                # copy name into PA df
                pa[1, i] <- members[1, i]
                
                # add blank row to top of pa df
                x <- rep(NA, ncol(pa))
                pa <- rbind(x, pa)
              }
              
            }
            
          }
        }
        
        
        # ********* IMPORTANCE = PRODUCTION *********
      } else {
        
        # if the current "role to check" is a pa, copy & paste the member onto the PA df,
        # remove the top row of the members df, and begin placing the next member.
        if (role_to_check == "Production Assistant") {
          
          for (i in 1:14) {
            # copy name into PA df
            pa[1, i] <- members[1, i]
          }
          
          # add blank row to top of pa df
          x <- rep(NA, ncol(pa))
          pa <- rbind(x, pa)
          
          # delete the top row off of the "members" df 
          members <- members[-1,]
          member_just_placed <- TRUE
          break
        }
        
        if (is.na(production[role_to_check, column_to_check])) {   # if the cell is empty in the "production" df,
          # THEN place member's name & info into the "production" df.
          for (i in 1:14) {
            production[starting_row_number, starting_column_number - 1 + i] <- members[1, i]
          }
          
          # "production" df is now updated with the new placement!
          # delete the top row off of the "members" df 
          members <- members[-1,]
          member_just_placed <- TRUE
          
        } else {    # however, if the row is NOT empty in the prod_df,
          
          # increment production_choice and role_choice according to their preference (production more important)
          role_choice <- role_choice + 1
          
          if (role_choice == 4) {
            
            production_choice <- production_choice + 1
            role_choice <- 1
            
            # if all choices are exhausted, add this member to the PA df
            if (role_choice == 1 && production_choice == 4) {
              for (i in 1:14) {
                # copy name into PA df
                pa[1, i] <- members[1, i]
                
                # add blank row to top of pa df
                x <- rep(NA, ncol(pa))
                pa <- rbind(x, pa)
              }
              
            }
            
          }
        }
        
      }
      
      if (member_just_placed) {break} # keep running this "repeat" loop while on the same member,
      # allows loop to remember the # of iteration
    }
  }
  # THEN, split the 1 big production df into 3 separate productions dfs
  production_A <- production[1:14]
  production_B <- production[15:28]
  production_C <- production[29:42]
  
  # create a list of all production dataframes for variable-free referencing
  prod_df_list <- list(production_A, production_B, production_C)
  
  
  # ********* PLACING PA's ON PRODUCTION DATAFRAMES *********
  # delete the top blank row
  pa <- pa[-1,]
  # sort PAs by #1 preferred production
  pa <- pa[order(pa$pref_prod_1),]
  
  # store indeces of unique values in pref_prod_1 column to find out where to split pa df
  unique_indexes <- tapply(seq_along(pa$pref_prod_1), pa$pref_prod_1, identity)[unique(pa$pref_prod_1)]
  unique_indexes <- lapply(unique_indexes, `[[`, 1) # only save first element from each list value
  unique_indexes <- unlist(unique_indexes, use.names = FALSE) # turn list into a vector
  
  
  for (i in 1:length(unique_indexes)) {
    
    if (i == length(unique_indexes)) {
      split <- pa[unique_indexes[i]:nrow(pa),]
    } else {
      split <- pa[unique_indexes[i]:(unique_indexes[i + 1] - 1),]
    }
    
    names(split) <- names(prod_df_list[[i]]) # change pa_df colnames to prod_df colnames to match for rbind fxn
    # moving row(s) matching production to that production's dataframe
    prod_df_list[[i]] <- rbind(prod_df_list[[i]], "Production Assistant" = split)
  }
  
  # ********* CREATING USABLE FILENAMES *********
  
filenames <- paste0(production_titles_u, ".csv")
  
  # ********* EXPORTING RESULTS *********

# returns prod. dataframes (1-3) and underscored filenames (4-6)
return(c(prod_df_list, filenames))
  
}

# Local Test
# testlist <- people_placer("Ace Ventura", "Blazing Saddles", "Contact", "TEST_ LUX Role Survey AU19 (Responses) - Form Responses 1.csv")

