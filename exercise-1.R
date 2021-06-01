
# ************************************************
# Introduction to Data Science with R: Exercise 1
# ************************************************



# ************************************************
# ----- Exercise 1: Activity 1 -----
# ************************************************

# Open a new script in RStudio and save it. Copy
# the code below and paste in your new script.
# Execute it line by line without selecting any
# code (start by putting your cursor on the first
# line). Execute code with CTRL-Enter or CMD-Enter.

# Then re-run the code by selecting the whole
# block and executing it. You're doing the same
# thing twice, the goal is to show you that you
# can run code with and without selecting it

# Don't worry about not knowing what this code does, 
# you will learn it later

# COPY CODE BELOW TO A NEW SCRIPT

# Load dplyr package
library(dplyr) # this will load starwars dataset

# Two ways to look at the data
head(starwars)
glimpse(starwars)

# Data manipulation
starwars_small <- select(starwars, name, height, homeworld)
starwars_human <- filter(starwars, species == "Human")




# *********************************
# ----- Exercise 1: Activity 2 ----
# *********************************

# Use the Tools pull down menu in RStudio to open
# Global Options. Change your Pane Layout so that
# your console is in the top left and your script
# (source) is in the top right.

# Keep the arrangement if you like or re-arrange
# as desired.



# ************************************************
# ----- Exercise 1: Activity 3 -----
# ************************************************

# There is a piece of code below that has been
# commented out (line begins with tmp <- ). There
# is an error in the code. 

# Uncomment the code (remove the #) and before fixing
# the code select the entire line and run it (although
# I showed above that you don't need to select a line to run
# it, in this case there is an error and if you don't select
# the full line it will try to run lines below so be sure
# to select the line to run it in this case).

# Look at the console, instead of showing the usual
# prompt (>) what does it show? Why? How do you
# get back to the prompt?

# Now, get back to the prompt, fix the code and run it. 
# Note that the console window needs to be active for you to
# use ESC (you need to click into the console then
# click ESC).


# UNCOMMENT LINE BELOW, SELECT THE FULL LINE AND RUN IT
#tmp <- filter(starwars, species %in% c("Human", "Droid))



# ************************************************
# ----- Exercise 1: Activity 4 -----
# ************************************************

# The code from the first question is pasted below.
# By running the head() and glimpse() lines of code
# (they will be discussed later) you can see that
# they give you different views of the data in
# the console.

# Use head() and/or glimpse() on the new objects
# created in the subsequent lines (in other words
# replace glimpse(starwars) with
# glimpse(starwars_small) etc). By looking at the
# code and looking at the output can you tell what
# select() and filter() are doing?


# Load dplyr package
library(dplyr) # this will load starwars dataset

# Two ways to look at the data
head(starwars)
glimpse(starwars)

# Data manipulation
starwars_small <- select(starwars, name, height, homeworld)
starwars_human <- filter(starwars, species == "Human")



# ************************************************
# ----- Exercise 1: Question 5 -----
# ************************************************

# This question assumes you've run the code from
# the previous questions.

# Look at your environment tab. What objects do you
# see? What happens if you click on starwars_human?
# What happens if you click on the little arrow
# next to starwars_human?




# ************************************************
# ----- Exercise 1: Activity 6 -----
# ************************************************


# Put your cursor in between the quotations below
# and press Tab on your keyboard. What does this
# do? After pressing Tab while your cursor is in
# between the quotations arrow down in the menu
# that pops up until you get to the data folder
# and click Tab again what does this do?


""


# ************************************************
# ----- Exercise 1: EXTRA CREDIT 1 -----
# ************************************************

# Revisit the Tools-Global-Appearance or 
# Tools-Global-Code options and explore more
# RStudio possible customizations.



