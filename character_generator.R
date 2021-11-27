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
if(!require(BMS))        install.packages("BMS",        repos = repo, dependencies = TRUE)

library(data.table)
library(tidyverse)
library(caret)
library(dplyr)
library(BMS)

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

## Set a universal seed to be used: ######################################################
seed <- 2021

### Basic Tables: ########################################################################
## Increment
# Half-Elves will receive random +1 increments
set.seed(seed, sample.kind = "Rounding")
he <- t(data.table(c(sample(c(1,1,0,0,0)),2)))
colnames(he) <- ability

inc_race <- data.table(Strength     = c(0,2,0,0,0,0,0,1,2,0,0,2,0),
                       Dextery      = c(0,0,2,2,2,2,2,1,0,1,0,0,0),
                       Constitution = c(2,2,0,0,0,0,1,1,0,0,1,1,0),
                       Intelligence = c(0,0,0,1,0,0,0,1,0,2,2,0,1),
                       Wisdom       = c(1,0,1,0,0,0,0,1,0,0,0,0,0),
                       Charisma     = c(0,0,0,0,1,1,0,1,1,0,0,0,2))
inc_race <- rbind(inc_race[1:11,],he,inc_race[12:13,])

rownames(inc_race) <- races
rm(he)

## Hit Points
hp <- c(12,8,8,8,10,8,10,10,8,6,8,6)

## Saving Throws
st <- data.table(Strength     = c(1,0,0,0,1,1,0,1,0,0,0,0),
                 Dextery      = c(0,1,0,0,0,1,0,1,1,0,0,0),
                 Constitution = c(1,0,0,0,1,0,0,0,0,1,0,0),
                 Intelligence = c(0,0,0,1,0,0,0,0,1,0,0,1),
                 Wisdom       = c(0,0,1,1,0,0,1,0,0,0,1,1),
                 Charisma     = c(0,1,1,0,0,0,1,0,0,1,1,0))

st_bin <- function(class){
  x <- which(classes == class)
  paste(st[x,1],st[x,2],st[x,3],st[x,4],st[x,5],st[x,6], sep = "")
}

## Skills
skill_background <- data.table(Acrobatics      = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
                               Animal_Handling = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
                               Arcana          = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
                               Athletics       = c(0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0),
                               Deception       = c(0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
                               History         = c(0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0),
                               Insight         = c(1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0),
                               Intimidation    = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
                               Investigation   = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                               Medicine        = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
                               Nature          = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                               Perception      = c(0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0),
                               Performance     = c(0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0),
                               Persuasion      = c(0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0),
                               Religion        = c(1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
                               Sleight_of_hand = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                               Stealth         = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1),
                               Survival        = c(0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0))

skill_class <- data.table(Acrobatics      = c(0,1,0,0,1,1,0,0,1,0,0,0),
                          Animal_Handling = c(1,1,0,1,1,0,0,1,0,0,0,0),
                          Arcana          = c(0,1,0,1,0,0,0,0,0,1,1,1),
                          Athletics       = c(1,1,0,0,1,1,1,1,1,0,0,0),
                          Deception       = c(0,1,0,0,0,0,0,0,1,1,1,0),
                          History         = c(0,1,1,0,1,1,0,0,0,0,1,1),
                          Insight         = c(0,1,1,1,1,1,1,1,1,1,0,1),
                          Intimidation    = c(1,1,0,0,1,0,1,0,1,1,1,0),
                          Investigation   = c(0,1,0,0,0,0,0,1,1,0,1,1),
                          Medicine        = c(0,1,1,1,0,0,1,0,0,0,0,0),
                          Nature          = c(1,1,0,1,0,0,0,1,0,0,1,0),
                          Perception      = c(1,1,0,1,1,0,0,1,1,0,0,0),
                          Performance     = c(0,1,0,0,0,0,0,0,1,0,0,0),
                          Persuasion      = c(0,1,1,0,0,0,1,0,1,1,0,0),
                          Religion        = c(0,1,1,1,0,1,1,0,0,1,1,1),
                          Sleight_of_hand = c(0,1,0,0,0,0,0,0,1,0,0,0),
                          Stealth         = c(0,1,0,0,0,1,0,1,1,0,0,0),
                          Survival        = c(1,1,0,1,1,0,0,1,0,0,0,0))

### Generator: ###########################################################################
## Dice:
set.seed(seed, sample.kind = "Rounding")
dice <- replicate(6, {
        dice <- sample(c(1:6), 4, replace = TRUE)
        dice <- dice[order(dice)][2:4]
        sum(dice)
})
dice <- dice[order(dice, decreasing = TRUE)]

## Functions
# Ability modifier:
mod <- function(dice){floor((dice - 10)/2)}

# Dice order based on class:
dice_order <- function(class){
  x <- if(class %in% classes[5:8]){c(sample(dice[1:2]),sample(dice[-c(1:2)]))
       }else{c(dice[1],sample(dice[-1]))}
  case_when(class %in% c("Barbarian", "Fighter")        ~ x,
            class %in% c("Bard", "Sorcerer", "Warlock") ~ x[c(2:6,1)],
            class %in% c("Cleric", "Druid")             ~ x[c(2:5,1,6)],
            class %in% c("Monk", "Ranger")              ~ x[c(3,1,4,5,2,6)],
            class == "Paladin"                          ~ x[c(1,3:6,2)],
            class == "Rogue"                            ~ x[c(2,1,3:6)],
            class == "Wizard"                           ~ x[c(2:4,1,5,6)])
}

## Primary choices:
set.seed(seed, sample.kind = "Rounding")
race       <- sample(races,1)
class      <- sample(classes,1)
background <- sample(backgrounds,1)
dice       <- dice_order(class)
alignment  <- sample(alignments,1)

# Skill choice:
n_choice <- c(2,3,2,2,2,2,2,3,4,2,2,2)[which(classes == class)]

character_skills <- as.data.frame(skill_class[which(classes == class),] - 
                                  skill_background[which(backgrounds == background)])
character_skills[character_skills < 0] <- 0

set.seed(seed, sample.kind = "Rounding")
sel_skills <- sample(which(character_skills == 1), n_choice)
sel_skills <- as.numeric(sel_skills[order(sel_skills)])

character_skills[,-sel_skills] <- 0
character_skills <- skill_background[which(backgrounds == background)] +
                    as.data.table(character_skills)

sp_bin <- paste(character_skills[1,1], character_skills[1,2], character_skills[1,3],
                character_skills[1,4], character_skills[1,5], character_skills[1,6],
                character_skills[1,7], character_skills[1,8], character_skills[1,9],
                character_skills[1,10],character_skills[1,11],character_skills[1,12],
                character_skills[1,13],character_skills[1,14],character_skills[1,15],
                character_skills[1,16],character_skills[1,17],character_skills[1,18],
                sep = "")



## Character sheet:  
ability_sheet <- t(data.frame(Dice = dice)) + inc_race[which(races == race),]

character_sheet <- cbind(data.frame(Race               = race,
                                    Class              = class,
                                    Background         = background,
                                    Alignment          = alignment,
                                    Level              = 1,
                                    Hit_Points         = hp[which(classes == class)] + 
                                                         mod(ability_sheet$Constitution),
                                    Armor_Class        = 10 + mod(ability_sheet$Dextery),
                                    Proficiency_Bonus  = 2,
                                    # Saving_Throws_Code is the representation of the 
                                    # binary that lists available saving throws.
                                    Saving_Throws_Code = st_bin(class),
                                    # Skill_Code is the representation in binary that 
                                    # lists chosen skills.
                                    Skill_Code         = sp_bin),
                         ability_sheet)


















##########################################################################################
