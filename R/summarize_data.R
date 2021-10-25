# summarize data and missing data

n <- c(data_PC_Long$N, 
       data_WL_Long$N,
       data_WH_Long$N,
       data_IH_Long$N)

# no NA's in the sex data

n_female <- c(sum(data_PC_Long$Sex == 1), 
              sum(data_WL_Long$Sex == 1),
              sum(data_WH_Long$Sex == 1),
              sum(data_IH_Long$Sex == 1))

n_long <- c(sum(data_PC_Long$Outcome),
            sum(data_WL_Long$Outcome),
            sum(data_WH_Long$Outcome),
            sum(data_IH_Long$Outcome))

n_short <- c(sum(data_PC_Short$Outcome),
             sum(data_WL_Short$Outcome),
             sum(data_WH_Short$Outcome),
             sum(data_IH_Short$Outcome))

n_married <- c(sum(data_PC_Married$Outcome),
               sum(data_WL_Married$Outcome),
               sum(data_WH_Married$Outcome),
               sum(data_IH_Married$Outcome))

n_curr_married <- c(sum(data_PC_Curr_Married$Outcome),
                    sum(data_WL_Curr_Married$Outcome),
                    sum(data_WH_Curr_Married$Outcome),
                    sum(data_IH_Curr_Married$Outcome))

age_range <- c(paste0(range(data_PC_Long$Age)[1], "-", range(data_PC_Long$Age)[2]), 
               paste0(range(data_WL_Long$Age)[1], "-", range(data_WL_Long$Age)[2]),
               paste0(range(data_WH_Long$Age)[1], "-", range(data_WH_Long$Age)[2]),
               paste0(range(data_IH_Long$Age)[1], "-", range(data_IH_Long$Age)[2]))

summary1 <- rbind(n, n_female, n_long, n_short, n_married, n_curr_married, age_range)

colnames(summary1) <- c("Coastal", "Lowland", "Highland", "Altiplano")
rownames(summary1) <- c("N", 
                        "women", 
                        "long-term ties", 
                        "short-term ties", 
                        "ever-married", 
                        "currently married", 
                        "age range")

print(xtable(summary1), 
      file = "output/tables/outcome_summary_site.txt", 
      only.contents = TRUE, 
      sanitize.rownames.function = function(x) x)

# make overview of study variables table

# variable

variable <- c("spousal age gap", 
              "preferred age gap (long)", 
              "preferred age gap (short)", 
              "number of surviving children",
              "mental health (depressed)",
              "mental health (nervous)", 
              "mental health (anxious)",
              "mental health (tired)",
              "mental health (worthless)", 
              "mental health (hopeless)",
              "age",
              "sex",
              "married",
              "wealth", 
              "education",
              "age at first marriage")

type <- c("outcome/predictor", "outcome", rep("", 8), "predictor", rep("", 5))

description <- c("age difference between focal individual and spouse", 
                 "age difference between focal individual and any nominated long-term partner",
                 "age difference between focal individual and any nominated short-term partner", 
                 "", 
                 "reported frequency of depression in last 30 days",
                 "reported frequency of nervousness in last 30 days",
                 "reported frequency of anxiety in last 30 days",
                 "reported frequency of tiredness in last 30 days",
                 "reported frequency of worthlessness in last 30 days",
                 "reported frequency of hopelessness in last 30 days",
                 "",
                 "", 
                 "whether individual is married or not",
                 "total value of household goods",
                 "number of years of education", 
                 "")

measure <- c("years", 
             "years", 
             "years", 
             "number of children", 
             "1-5 Likert scale",
             "1-5 Likert scale",
             "1-5 Likert scale",
             "1-5 Likert scale",
             "1-5 Likert scale",
             "1-5 Likert scale",
             "years",
             "man/woman",
             "yes/no", 
             "wealth units",
             "years",
             "years")

variable_desc <- cbind(variable, type, description, measure)

print(xtable(variable_desc), 
      file = "output/tables/variable_description.txt", 
      include.rownames = FALSE, 
      include.colnames = FALSE)

# PC

list_PC <- list(data_PC_Curr_Married$AgeDiff[data_PC_Curr_Married$Outcome == 1], 
                data_PC_Long$AgeDiff[data_PC_Long$Outcome == 1],
                data_PC_Short$AgeDiff[data_PC_Short$Outcome == 1],
                data_PC_Short$ChildrenAlive,
                data_PC_Short$Sad,
                data_PC_Short$Nervous,
                data_PC_Short$Anxious,
                data_PC_Short$Tired,
                data_PC_Short$Worthless,
                data_PC_Short$Hopeless,
                data_PC_Short$Age, 
                data_PC_Short$Sex,
                as.integer(as.factor(data_PC_Short$Married)),
                data_PC_Short$GoodsValues,
                data_PC_Short$Edu,
                data_PC_Short$AgeAtFM)

median <- unlist(lapply(list_PC, function(x) median(x, na.rm = TRUE)))
median_PC <- round(median, 1)

min <- unlist(lapply(list_PC, function(x) min(x, na.rm = TRUE)))
min_PC <- round(min, 1)
max <- unlist(lapply(list_PC, function(x) max(x, na.rm = TRUE)))
max_PC <- round(max, 1)

n_missing_PC <- c("", 
                  "", 
                  "",
                  sum(is.na(data_PC_Short$ChildrenAlive)),
                  sum(is.na(data_PC_Short$Sad)),
                  sum(is.na(data_PC_Short$Nervous)),
                  sum(is.na(data_PC_Short$Anxious)),
                  sum(is.na(data_PC_Short$Tired)),
                  sum(is.na(data_PC_Short$Worthless)),
                  sum(is.na(data_PC_Short$Hopeless)),
                  0,
                  "", 
                  sum(is.na(data_PC_Short$Married)), 
                  sum(is.na(data_PC_Short$GoodsValues)), 
                  sum(is.na(data_PC_Short$EducationYears)), 
                  sum(is.na(data_PC_Short$AgeAtFM)))

n_missing_PC[n_missing_PC == 0] <- ""

# WL

list_WL <- list(data_WL_Curr_Married$AgeDiff[data_WL_Curr_Married$Outcome == 1], 
                data_WL_Long$AgeDiff[data_WL_Long$Outcome == 1],
                data_WL_Short$AgeDiff[data_WL_Short$Outcome == 1],
                data_WL_Short$ChildrenAlive,
                data_WL_Short$Sad,
                data_WL_Short$Nervous,
                data_WL_Short$Anxious,
                data_WL_Short$Tired,
                data_WL_Short$Worthless,
                data_WL_Short$Hopeless,
                data_WL_Short$Age, 
                data_WL_Short$Sex,
                as.integer(as.factor(data_WL_Short$Married)),
                data_WL_Short$GoodsValues,
                data_WL_Short$Edu,
                data_WL_Short$AgeAtFM)

median <- unlist(lapply(list_WL, function(x) median(x, na.rm = TRUE)))
median_WL <- round(median, 1)

min <- unlist(lapply(list_WL, function(x) min(x, na.rm = TRUE)))
min_WL <- round(min, 1)
max <- unlist(lapply(list_WL, function(x) max(x, na.rm = TRUE)))
max_WL <- round(max, 1)

# get the missing values

n_missing_WL <- c("", 
                  "", 
                  "",
                  sum(is.na(data_WL_Short$ChildrenAlive)),
                  sum(is.na(data_WL_Short$Sad)),
                  sum(is.na(data_WL_Short$Nervous)),
                  sum(is.na(data_WL_Short$Anxious)),
                  sum(is.na(data_WL_Short$Tired)),
                  sum(is.na(data_WL_Short$Worthless)),
                  sum(is.na(data_WL_Short$Hopeless)),
                  sum(is.na(data_WL_Short$Age)),
                  sum(is.na(data_WL_Short$Sex)), 
                  sum(is.na(data_WL_Short$Married)), 
                  sum(is.na(data_WL_Short$GoodsValues)), 
                  sum(is.na(data_WL_Short$Edu)), 
                  sum(is.na(data_WL_Short$AgeAtFM)))

n_missing_WL[n_missing_WL == 0] <- ""

# WH

list_WH <- list(data_WH_Curr_Married$AgeDiff[data_WH_Curr_Married$Outcome == 1], 
                data_WH_Long$AgeDiff[data_WH_Long$Outcome == 1],
                data_WH_Short$AgeDiff[data_WH_Short$Outcome == 1],
                data_WH_Short$ChildrenAlive,
                data_WH_Short$Sad,
                data_WH_Short$Nervous,
                data_WH_Short$Anxious,
                data_WH_Short$Tired,
                data_WH_Short$Worthless,
                data_WH_Short$Hopeless,
                data_WH_Short$Age, 
                data_WH_Short$Sex,
                as.integer(as.factor(data_WH_Short$Married)),
                data_WH_Short$GoodsValues,
                data_WH_Short$Edu,
                data_WH_Short$AgeAtFM)

median <- unlist(lapply(list_WH, function(x) median(x, na.rm = TRUE)))
median_WH <- round(median, 1)

min <- unlist(lapply(list_WH, function(x) min(x, na.rm = TRUE)))
min_WH <- round(min, 1)
max <- unlist(lapply(list_WH, function(x) max(x, na.rm = TRUE)))
max_WH <- round(max, 1)

# get the missing values

n_missing_WH <- c("", 
                  "", 
                  "",
                  sum(is.na(data_WH_Short$ChildrenAlive)),
                  sum(is.na(data_WH_Short$Sad)),
                  sum(is.na(data_WH_Short$Nervous)),
                  sum(is.na(data_WH_Short$Anxious)),
                  sum(is.na(data_WH_Short$Tired)),
                  sum(is.na(data_WH_Short$Worthless)),
                  sum(is.na(data_WH_Short$Hopeless)),
                  sum(is.na(data_WH_Short$Age)),
                  sum(is.na(data_WH_Short$Sex)), 
                  sum(is.na(data_WH_Short$Married)), 
                  sum(is.na(data_WH_Short$GoodsValues)), 
                  sum(is.na(data_WH_Short$Edu)), 
                  sum(is.na(data_WH_Short$AgeAtFM)))

n_missing_WH[n_missing_WH == 0] <- ""

# IH

list_IH <- list(data_IH_Curr_Married$AgeDiff[data_IH_Curr_Married$Outcome == 1], 
                data_IH_Long$AgeDiff[data_IH_Long$Outcome == 1],
                data_IH_Short$AgeDiff[data_IH_Short$Outcome == 1],
                data_IH_Short$ChildrenAlive,
                data_IH_Short$Sad,
                data_IH_Short$Nervous,
                data_IH_Short$Anxious,
                data_IH_Short$Tired,
                data_IH_Short$Worthless,
                data_IH_Short$Hopeless,
                data_IH_Short$Age, 
                data_IH_Short$Sex,
                as.integer(as.factor(data_IH_Short$Married)),
                data_IH_Short$GoodsValues,
                data_IH_Short$Edu,
                data_IH_Short$AgeAtFM)

median <- unlist(lapply(list_IH, function(x) median(x, na.rm = TRUE)))
median_IH <- round(median, 1)

min <- unlist(lapply(list_IH, function(x) min(x, na.rm = TRUE)))
min_IH <- round(min, 1)
max <- unlist(lapply(list_IH, function(x) max(x, na.rm = TRUE)))
max_IH <- round(max, 1)

# get the missing values

n_missing_IH <- c("", 
                  "", 
                  "",
                  sum(is.na(data_IH_Short$ChildrenAlive)),
                  sum(is.na(data_IH_Short$Sad)),
                  sum(is.na(data_IH_Short$Nervous)),
                  sum(is.na(data_IH_Short$Anxious)),
                  sum(is.na(data_IH_Short$Tired)),
                  sum(is.na(data_IH_Short$Worthless)),
                  sum(is.na(data_IH_Short$Hopeless)),
                  5, # 5 individuals ages were estimated based on photos
                  sum(is.na(data_IH_Short$Sex)), 
                  sum(is.na(data_IH_Short$Married)), 
                  sum(is.na(data_IH_Short$GoodsValues)), 
                  sum(is.na(data_IH_Short$Edu)), 
                  sum(is.na(data_IH_Short$AgeAtFM)))

n_missing_IH[n_missing_IH == 0] <- ""

variable_table_PC <- cbind(variable, median_PC, min_PC, max_PC, n_missing_PC)
variable_table_WL <- cbind(variable, median_WL, min_WL, max_WL, n_missing_WL)
variable_table_WH <- cbind(variable, median_WH, min_WH, max_WH, n_missing_WH)
variable_table_IH <- cbind(variable, median_IH, min_IH, max_IH, n_missing_IH)

colnames(variable_table_PC) <- colnames(variable_table_WL) <- colnames(variable_table_WH) <- colnames(variable_table_IH) <- c("variable", "median", "min", "max", "n missing")

print(xtable(variable_table_PC), 
      file = "output/tables/variable_table_1.txt", 
      include.rownames = FALSE, 
      include.colnames = FALSE)

print(xtable(variable_table_WL), 
      file = "output/tables/variable_table_2.txt", 
      include.rownames = FALSE, 
      include.colnames = FALSE)

print(xtable(variable_table_WH), 
      file = "output/tables/variable_table_3.txt", 
      include.rownames = FALSE, 
      include.colnames = FALSE)

print(xtable(variable_table_IH), 
      file = "output/tables/variable_table_4.txt", 
      include.rownames = FALSE, 
      include.colnames = FALSE)

