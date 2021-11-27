### Code Summary: ########################################################################
# This code generates a character for the combat simulation.
# This is based on the D&D Player's Handbook, 5th Edition.
#
### Basic packages: ######################################################################
repo <- "http://cran.us.r-project.org"

# Required packages:
if(!require(data.table)) install.packages("data.table", repos = repo, dependencies = TRUE)
if(!require(tidyverse))  install.packages("tidyverse",  repos = repo, dependencies = TRUE)
if(!require(caret))      install.packages("caret",      repos = repo, dependencies = TRUE)
if(!require(dplyr))      install.packages("dplyr",      repos = repo, dependencies = TRUE)

library(data.table)
library(tidyverse)
library(caret)
library(dplyr)

### Basic Lists: #########################################################################
races       <- c("Hill-Dwarf", "Mountain-Dwarf", "Wood-Elf", "High-Elf", "Drow",
                 "Lightfoot-Halfling", "Stout-Halfling", "Human", "Dragonborn", 
                 "Forest-Gnome", "Rock-Gnome", "Half-Elf", "Half-Orc", "Tiefling")

classes     <- c("Barbarian", "Bard", "Cleric", "Druid", "Fighter", "Monk", "Paladin",
                 "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard")

backgrounds <- c("Acolyte", "Charlatan", "Criminal/Spy", "Entertainer",
                 "Folk Hero", "Gladiator", "Guild-Artisan/Guild-Merchant",
                 "Hermit", "Knight", "Noble", "Outlander", "Pirate", "Sage",
                 "Sailor", "Soldier", "Urchin")

alignments  <- c("Lawfull-Good"   , "Neutral-Good", "Chaotic-Good",
                 "Lawfull-Neutral", "True-Neutral", "Chaotic-Neutral",
                 "Lawfull-Evil"   , "Neutral-Evil", "Chaotic-Evil")

sex         <- c("Male", "Female")

ability     <- c("Strength", "Dextery", "Constitution",
                 "Intelligence", "Wisdom", "Charisma")


### Generator: ###########################################################################
# Set a universal seed to be used:
seed <- 2021

# Modifier function
mod <- function(d){floor((d - 10)/2)}

# Dice:
set.seed(seed, sample.kind = "Rounding")
dice <- replicate(6, {
        dice <- sample(c(1:6), 4, replace = TRUE)
        dice <- dice[order(dice)][2:4]
        sum(dice)
})

# Primary choices:
set.seed(seed, sample.kind = "Rounding")
race       = sample(races,1)
class      = sample(classes,1)
background = sample(backgrounds,1)

# Dice order based on class:


# Character sheet:
set.seed(seed, sample.kind = "Rounding")
character_sheet <- data.frame(Race       = race,
                              Class      = class,
                              Background = background)


















##########################################################################################