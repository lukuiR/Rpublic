#########     WORKFORCE FORECASTING TOOL: INITIAL SETUP     ##########

# This is the initial setup code for the workforce forecasting tool.

# It should be run the first time the program is used on a computer, to install
# the necessary packages required for the tool to run.

# After this it does not need to be run again

# You can run the setup code either by using the shortcut ctrl+shift+enter , 
# or by clicking the "Run" button in the top right of the window

# You will know it has finished when the red stop sign disappears from the top
# right of the console window at the bottom

install.packages(c("tidyverse","rlang","scales","gridExtra","ggrepel","shinyWidgets","shinydashboard","DT","plotly","reshape2","readr","shinyjs","shinyBS"))