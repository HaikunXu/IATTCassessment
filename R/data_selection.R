#' Generate area code for longline
#' 
#' \code{data_selection} This function filter the data for LL CPUE standardization
#' 
#' @export

# Data selection criterion for JPN operational LL data in the EPO
# Inspired by http://www.iotc.org/sites/default/files/documents/2018/10/IOTC-2018-WPM09-12_Rev1.pdf
# Haikun Xu; 1/30/2019

select_data <- function(data,c1,c2,c3,c4) {

# c1: minimal setsbyvessel
# c2: minimal quartersbyvessel
# c3: minimal setsbygrid
# c4: minimal quartersbygrid

  
# Generate the quantities for data selection
Data <- data %>% 
  group_by(Vessel) %>% mutate(setsbyvessel=n(), # total number of sets per vessel
                                quartersbyvessel=length(unique(Year))) %>% # total number of quarters fished per vessel
  group_by(Lat,Lon) %>% mutate(setsbygrid=n(), # total number of sets per grid cell
                               quartersbygrid=length(unique(Year))) # # total number of quarters per grid cell

# Data selection
data_selected <- Data %>% filter(setsbyvessel>=c1, # select vessel
                                quartersbyvessel>=c2, # select vessel
                                setsbygrid>=c3, # select grid
                                quartersbygrid>=c4) # select grid

return(data_selected)
}