#- - - - - - - - - - - - - - - - - - -#
#                                     #
#       Good coding practice          #
#           Good example              #
#                                     #
#       author: Romy Zeiss            #
#         date: 2022-11-08            #
#                                     #
#- - - - - - - - - - - - - - - - - - -#

# This code presents the improved version of "Script_raw.R" using 
# good coding practices.
 

#- - - - - - - - - - - - - - - - - - -
## Set working directory ####
#- - - - - - - - - - - - - - - - - - -

# please set your working directory using setwd()

# Note: 
# 4x "#" at the end of a line will make a header that can be used in RStudio


#- - - - - - - - - - - - - - - - - - -
## Load required packages ####
#- - - - - - - - - - - - - - - - - - -

library(tidyverse)


#- - - - - - - - - - - - - - - - - - -
## Load data ####
#- - - - - - - - - - - - - - - - - - -
# We will use data that are included in the tidyverse packages.

# load properties of different car types
data(mtcars)
head(mtcars)

#- - - - - - - - - - - - - - - - - - -
## Define functions ####
#- - - - - - - - - - - - - - - - - - -

# investigate linear relationship between variable 1 and variable 2
lm_2_vars <- function( var_response, var_predictor, input_data ){
  
  # var_response:   response variable
  # var_predictor:  predictor
  # input_data:     data set in long format
  
  ## Check input objects
  if( !(var_response %in% colnames(input_data)) ) {
    cat("Please check the name of your response variable.", sep="\n")
  
  } else { 
  
    if( !(var_predictor %in% colnames(input_data)) ) {
      cat("Please check the name of your predictor column.", sep="\n")
    
    } else {
      
      if( var_response == var_predictor ){
        
        cat("Please check your variables. Response and predictor variable seem to be exactly the same.", sep="\n")
        
      } else {
      
          if( is.character(input_data[,var_response]) ) {
            cat("Note: Response variable will be treated as factor.", sep="\n")
          
          } else {
            
            cat("Note: Response variable will be treated as numeric.", sep="\n")
          
          }
      
          if( is.character(input_data[,var_predictor]) ) {
            cat("Note: Predictor variable will be treated as factor.", sep="\n")
          
          } else {
            
            cat("Note: Predictor variable will be treated as numeric.", sep="\n")
          
          }
          
    
        ## Main purpose of the function (actual analysis)
        summary( lm( get(var_response) ~ get(var_predictor), 
                     data = input_data ) )
        
        # produce scatterplot with regression line
        print({
          ggplot(data=input_data,
               aes(x=get(var_predictor), y=get(var_response)))+
            geom_point()+
            geom_smooth()
        })
      }
    }
  }
}


#- - - - - - - - - - - - - - - - - - -
## Explore data ####
#- - - - - - - - - - - - - - - - - - -

# check how variable cyl is distributed
hist(mtcars$cyl)

# what kind of variable is cyl (i.e., numeric, factor, date)?
str(mtcars$cyl)

# view correlations of all variables
plot(mtcars)

#- - - - - - - - - - - - - - - - - - -
## Analyse data ####
#- - - - - - - - - - - - - - - - - - -

### Influence of cyl on other variables ####

# define response variables
response_variables <- c("mpg", "disp", "hp", "vs", "gear")

# test for linear effects of cyl on response variables
for( variable in response_variables ) {
  
  print("---------------------------------------------")
  print( paste("Analysing variable", variable, "now.") )
  
  lm_2_vars(var_response = variable, var_predictor = "cyl",
            input_data = mtcars)
  
}





