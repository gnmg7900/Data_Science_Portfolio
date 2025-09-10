#------------------------------------------------------------------------ #
#     Insurance Data Science Assignment
#     Data Science for Finance 2024-25
#     Francisco Perestrello 20241560 | Gonçalo Gomes 20211007 | 
#     Nuno Vieira 20241111 | Petr Terletskiy 20211580
#------------------------------------------------------------------------ #

# ------------------------------------------------------------------------
#                                 Part I
# ------------------------------------------------------------------------

rm(list=ls(all=TRUE))

# Reading the data files

portfolio=read.table(file.choose(),header=TRUE,sep=";")  #choose autodata.txt
names(portfolio)
nrow(portfolio)

claims=read.table(file.choose(),header=TRUE,sep=";")  #choose claimsdatanew.txt
names(claims)
nrow(claims)

# Claim Severity for Third Party Liability coverage

TPclaims=claims[claims$coverage=="1RC",]
nrow(TPclaims)


# Organizing the data in appropriate files for modeling Claim Frequency and Severity

# Number of Claims per Policy

T=table(TPclaims$ncontract)
T1=as.numeric(names(T))
T2=as.numeric(T)
n1 = data.frame(ncontract=T1,nclaims=T2)
I = portfolio$ncontract%in%T1
T1=portfolio$ncontract[I==FALSE]
n2 = data.frame(ncontract=T1,nclaims=0)
number=rbind(n1,n2)
table(number$nclaims)

# Organize data in appropriate files for modeling Claim Frequency and Severity

# Frequency
baseFREQ = merge(portfolio,number)
head(baseFREQ)
nrow(baseFREQ)

# Severity
baseSEV=merge(portfolio,TPclaims) 
tail(baseSEV)
nrow(baseSEV)
          

# ------------------------------------------------------------------------
#                 1. Exploratory Analysis of Claim Counts
# ------------------------------------------------------------------------

# Install a package if needed, then load them 
for (pkg in c("Hmisc", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
}

# Visualization functions
describe_with_sd_n_val <- function(variable, var_name = deparse(substitute(variable))) {
    cat("\n", "> Detailed Distribution Analysis:", "\n")
    print(describe(variable))
    
    sd_val <- sd(variable, na.rm = TRUE)
    var_val <- var(variable, na.rm = TRUE)
    cat("\n", "> Spread Measures:", "\n")
    cat(sprintf("%-22s: %.4f\n", "Standard Deviation", sd_val))
    cat(sprintf("%-22s: %.4f\n", "Variance", var_val))
}

plot_numerical <- function(data, var_name) {
    # Create histogram
    hist(data[[var_name]],
         main = paste("Distribution of", var_name),
         xlab = var_name,
         ylab = "Frequency", 
         col = 'blue')
    
    # Create boxplot
    boxplot(data[[var_name]], 
            main = paste("Distribution of", var_name),
            xlab = var_name,
            horizontal = TRUE)
}


### ncontract variable
describe(baseFREQ$ncontract)
length(unique(baseFREQ$ncontract)) == nrow(baseFREQ) 
# if the statement above is TRUE, 
# then there are no duplicates


### nclaims variable
describe_with_sd_n_val(baseFREQ$nclaims) # there is a person with 16 claims (outlier)
mean(baseFREQ$nclaims == 0) * 100 # proportion of people with no claims
plot_numerical(baseFREQ, "nclaims")
# there is an observation with 16 number of claims
# we will remove this observation later as it is an outlier


### exposition variable 
describe_with_sd_n_val(baseFREQ$exposition)
plot_numerical(baseFREQ, "exposition")

# Average number of claims per client per year
N<-baseFREQ$nclaims
E<-baseFREQ$exposition
total_claims <- sum(N)
total_exposure <- sum(E)
lambda <- total_claims/total_exposure
lambda_variance <- weighted.mean((N/E - lambda)^2, E)

cat("PORTFOLIO CLAIM FREQUENCY ANALYSIS\n",
    "==================================\n",
    sprintf("%-25s: %d\n", "Total Claims", total_claims),
    sprintf("%-25s: %.2f policy-years\n", "Total Exposure", total_exposure),
    sprintf("\n%-25s: %.6f\n", "Average Claim Frequency", lambda),
    sprintf("%-25s: %.6f\n", "Frequency Variance", lambda_variance),
    sep = "")  


### bonus variable (will not be used in the model)
describe_with_sd_n_val(baseFREQ$bonus)
hist(baseFREQ$bonus,
     main = paste("Distribution of bonus"),
     xlab = "bonus",
     ylab = "Frequency", 
     col = 'blue')


### agedriver variable
describe_with_sd_n_val(baseFREQ$agedriver)
plot_numerical(baseFREQ, "agedriver")

### popdensity variable (will not be used in the model)
describe_with_sd_n_val(baseFREQ$popdensity)
hist(baseFREQ$popdensity,
     main = paste("Distribution of popdensity"),
     xlab = "popdensity",
     ylab = "Frequency", 
     col = 'blue')


### region variable (will not be used in the model)
describe(baseFREQ$region)
hist(baseFREQ$region,
     main = paste("Distribution of region"),
     xlab = "region",
     ylab = "Frequency", 
     col = 'blue')


### zone variable
zone_factor<-factor(baseFREQ$zone)
levels(zone_factor)
describe(baseFREQ$zone)
ggplot(data = baseFREQ, aes(x = zone, fill = zone)) +
    geom_bar() +
    labs(title = "Frequency of zones",
         x = "Zone Category",
         y = "Count")

# Number of claims in each of the Zone variable Levels
tapply(N, zone_factor, sum)
# Claim Frequency for type of zone
tapply(N, zone_factor, sum)/tapply(E, zone_factor, sum)


### brand variable
brand_factor<-factor(baseFREQ$brand)
levels(brand_factor)
describe(baseFREQ$brand)
ggplot(data = baseFREQ, aes(x = brand)) +
    geom_bar() +
    labs(title = "Frequency of brands",
         x = "Brand Category",
         y = "Count")

# Number of claims in each of the Brand variable Levels
tapply(N, brand_factor, sum)
# Claim Frequency for type of brand
tapply(N, brand_factor, sum)/tapply(E, brand_factor, sum)


### agevehicle variable
describe_with_sd_n_val(baseFREQ$agevehicle)
plot_numerical(baseFREQ, "agevehicle")

### fuel variable
fuel_factor<-factor(baseFREQ$fuel)
levels(fuel_factor)
describe(baseFREQ$fuel)
ggplot(data = baseFREQ, aes(x = fuel)) +
    geom_bar() +
    labs(title = "Frequency of fuel",
         x = "Fuel Category",
         y = "Count")

# Number of claims in each of the Fuel variable Levels
tapply(N, fuel_factor, sum)
# Claim Frequency for type of fuel
tapply(N, fuel_factor, sum)/tapply(E, fuel_factor, sum)


### power variable
power_factor<-factor(baseFREQ$power)
levels(power_factor)
describe(baseFREQ$power)
ggplot(data = baseFREQ, aes(x = power)) +
    geom_bar() +
    labs(title = "Frequency of power",
         x = "Power Category",
         y = "Count")
# Number of claims in each of the Power variable Levels
tapply(N, power_factor, sum)
# Claim Frequency for type of power
tapply(N, power_factor, sum)/tapply(E, power_factor, sum)


# ------------------------------------------------------------------------
#                 2. Descriptive Analysis of Claim Severity
# ------------------------------------------------------------------------

names(baseSEV)

# Cost
describe_with_sd_n_val(baseSEV$cost)
summary(baseSEV$cost)
quantile(baseSEV$cost,prob=c(0.5,0.9,0.95,0.99))
boxplot(baseSEV$cost, main="Claim Costs BoxPlot",horizontal=TRUE, col="dodgerblue")
hist(baseSEV$cost,
     main = "Histogram of Claim Costs",
     col = "skyblue",
     xlab = "Cost",
     ylab = "Frequency",
     breaks = 100,            
     xlim = c(min(baseSEV$cost), max(baseSEV$cost)))

min(baseSEV$cost) # negative values -> adjustments between insurances
max(baseSEV$cost)

hist(baseSEV$cost[baseSEV$cost <= 25], # Focus on adjustments between insurances
     main = "Claim Costs ≤ 25",
     col = "lightblue",
     xlab = "Cost",
     ylab = "Frequency",
     breaks = seq(-4000, 100, by = 100))


baseSEV=baseSEV[baseSEV$cost>25,] # remove instances where client was not at fault and adjustment between insurances
nrow(baseSEV) # 790 rows removed

describe_with_sd_n_val(baseSEV$cost)
summary(baseSEV$cost)
plot_numerical(baseSEV, "cost")

hist(baseSEV$cost,
     main = "Histogram of Claim Costs",
     col = "skyblue",
     xlab = "Cost",
     ylab = "Frequency",
     breaks = 50,            
     xlim = c(min(baseSEV$cost), max(baseSEV$cost)))

# Categorical functions
analyze_severity_by_cat <- function(varname) {
  X <- factor(baseSEV[[varname]])
  Y <- baseSEV$cost
  
  result <- data.frame(
    Level = levels(X),
    MeanCost = tapply(Y, X, mean),
    SDCost = tapply(Y, X, sd),
    NumClaims = as.vector(table(X))
  )
  print(result[order(-result$MeanCost), ])  # Sort by descending severity
}

plot_severity_bar <- function(varname) {
  X <- factor(baseSEV[[varname]])
  Y <- baseSEV$cost
  mean_cost <- tapply(Y, X, mean)
  
  barplot(mean_cost,
          main = paste("Claim Severity by", varname),
          col = "firebrick",
          ylab = "Average Claim Cost",
          las = 2)
}

# Numerical/Categorical function 

interactiongraphic_severity <- function(title = "Claim Severity vs Group",
                                        name = "agedriver",
                                        lev,
                                        contin = TRUE) {
  
  if (contin == TRUE) {
    # Continuous variable: bin using cut()
    X <- cut(as.numeric(baseSEV[[name]]), breaks = lev, include.lowest = TRUE) 
  } else {
    # Categorical variable: convert to factor
    X <- as.factor(baseSEV[[name]])
  }
  
  Y <- baseSEV$cost
  FREQ <- levels(X)
  mea <- variance <- n <- rep(NA, length(FREQ))
  
  for (k in 1:length(FREQ)) {
    values <- Y[X == FREQ[k]]
    mea[k] <- mean(values, na.rm = TRUE)
    variance[k] <- var(values, na.rm = TRUE)
    n[k] <- sum(!is.na(values))
  }
  
  # Barplot of claim counts
  w <- barplot(n,
               names.arg = FREQ,
               col = "light blue",
               axes = FALSE,
               xlim = c(0, 1.2 * length(FREQ) + 0.5))
  mid <- w[, 1]
  axis(2)
  par(new = TRUE)
  
  # Confidence intervals
  IC1 <- mea + 1.96 * sqrt(variance / n)
  IC2 <- mea - 1.96 * sqrt(variance / n)
  globalmean <- mean(Y, na.rm = TRUE)
  
  # Line + CI plot
  plot(mid, mea,
       main = title,
       ylim = range(c(IC1, IC2), na.rm = TRUE),
       type = "b",
       col = "red",
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = c(0, 1.2 * length(FREQ) + 0.5))
  
  segments(mid, IC1, mid, IC2, col = "red")
  segments(mid - 0.1, IC1, mid + 0.1, IC1, col = "red")
  segments(mid - 0.1, IC2, mid + 0.1, IC2, col = "red")
  points(mid, mea, pch = 19, col = "red")
  axis(4)
  abline(h = globalmean, lty = 2, col = "red")
  
  # Labels
  mtext("Number of Claims", 2, line = 2, cex = 1.2, col = "light blue")
  mtext("Average Claim Cost", 4, line = -2, cex = 1.2, col = "red")
}

# Driver age
describe_with_sd_n_val(baseSEV$agedriver)
plot_numerical(baseSEV, "agedriver")

age <- seq(18, 100, by = 1)  # sequence 18 to 100 (1 year)
SEV <- rep(NA, length(age))  # stores average claim cost for each age
for(k in 1:length(age)) {                     # Loop
  I <- baseSEV$agedriver == age[k]    # logical filter
  X <- baseSEV$cost[I]                # mean cost for that age group
  SEV[k] <- mean(X)
}
plot(age, SEV,
     main = "Age of Driver vs Claim Severity",
     xlab = "Age",
     ylab = "Claim Severity",
     col = "firebrick",
     pch = 19)

baseSEV$age_bin <- cut(baseSEV$agedriver,
                       breaks = c(18, 24, 34, 44, 64, 100),
                       right = FALSE,
                       labels = c("18–24", "25–34", "35–44", "45–64", "65+"))
agg_sev <- aggregate(cost ~ age_bin, data = baseSEV, mean)
barplot(agg_sev$cost,
        names.arg = agg_sev$age_bin,
        main = "Binned Age of Driver vs Claim Severity",
        xlab = "Age Group",
        ylab = "Avg Claim Severity",
        col = "firebrick")

Q <- quantile(baseSEV$agedriver, probs = (0:10)/10, na.rm = TRUE)
Q[1] <- Q[1] - 1
baseSEV$age_bin <- cut(baseSEV$agedriver, breaks = Q, include.lowest = TRUE) # include.lowest = TRUE?
agg_sev <- aggregate(cost ~ age_bin, data = baseSEV, mean)
barplot(agg_sev$cost,
        names.arg = agg_sev$age_bin,
        main = "Quantile-Binned Age vs Claim Severity",
        xlab = "Age Decile Group",
        ylab = "Avg Claim Severity",
        col = "firebrick")

# Vehicle age
describe_with_sd_n_val(baseSEV$agevehicle)
plot_numerical(baseSEV, "agevehicle")

veh_age <- sort(unique(baseSEV$ageveh))  # sorted vector of distinct vehicle ages
SEV_veh <- rep(NA, length(veh_age))              # store severity per vehicle age
for(k in 1:length(veh_age)) {
  I <- baseSEV$ageveh == veh_age[k]
  X <- baseSEV$cost[I]
  SEV_veh[k] <- mean(X, na.rm = TRUE)
}
plot(veh_age, SEV_veh,
     main = "Age of Vehicle vs Claim Severity",
     xlab = "Vehicle Age",
     ylab = "Claim Severity",
     col = "firebrick",
     pch = 19)

baseSEV$veh_age_bin <- cut(baseSEV$ageveh,
                           breaks = c(0, 2, 5, 10, 15, 25, 100),
                           right = FALSE,
                           labels = c("0–2", "3–5", "6–10", "11–15", "16–25", "26+"))
agg_sev_veh <- aggregate(cost ~ veh_age_bin, data = baseSEV, mean)
barplot(agg_sev_veh$cost,
        names.arg = agg_sev_veh$veh_age_bin,
        main = "Binned Vehicle Age vs Claim Severity",
        xlab = "Vehicle Age Group",
        ylab = "Avg Claim Severity",
        col = "firebrick")

Q_veh <- quantile(baseSEV$ageveh, probs = (0:10)/10, na.rm = TRUE)
Q_veh[1] <- Q_veh[1] - 1
Q_veh <- unique(Q_veh)
baseSEV$veh_age_bin <- cut(baseSEV$ageveh,
                           breaks = Q_veh,
                           include.lowest = TRUE)
agg_sev_veh <- aggregate(cost ~ veh_age_bin, data = baseSEV, mean)
barplot(agg_sev_veh$cost,
        names.arg = agg_sev_veh$veh_age_bin,
        main = "Quantile-Binned Vehicle Age vs Claim Severity",
        xlab = "Vehicle Age Decile",
        ylab = "Avg Claim Severity",
        col = "firebrick")

# Zone -> not significant
zone_factor_sev <- factor(baseSEV$zone)
levels(zone_factor_sev)
describe(baseSEV$zone)
analyze_severity_by_cat("zone")
plot_severity_bar("zone")

# Power -> it deviates between the different power, but nothing significant
power_factor_sev <- factor(baseSEV$power)
levels(power_factor_sev)
describe(baseSEV$power)
describe_with_sd_n_val(baseSEV$power)
analyze_severity_by_cat("power")
plot_severity_bar("power")

# Brand -> there is a substantial difference on the avg claim cost for brand 13
brand_factor_sev <- factor(baseSEV$brand)
levels(brand_factor_sev)
describe(baseSEV$brand)
describe_with_sd_n_val(baseSEV$brand)
analyze_severity_by_cat("brand")
plot_severity_bar("brand")

# Fuel -> D has a slightly higher average cost
fuel_factor_sev <- factor(baseSEV$fuel)
levels(fuel_factor_sev)
describe(baseSEV$fuel)
analyze_severity_by_cat("fuel")
plot_severity_bar("fuel")


# ------------------------------------------------------------------------
#               3. Graphical Analysis of Variable Interactions
# ------------------------------------------------------------------------

######### Claim Count Analysis #########

# Install a package if needed, then load them 
for (pkg in c("Hmisc", "ggplot2", "dplyr", "corrplot", "reshape2", "gridExtra")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
}


# Create custom vehicle age categories
baseFREQ$veh_age_cat <- cut(baseFREQ$agevehicle,
                            breaks = c(0, 2, 5, 10, 15, 25, 100),
                            labels = c("[0;2[", "[2;5[","[5;10[",
                                       "[10;15[", "[15;25[","[25;50["),
                            right = FALSE,
                            include.lowest = TRUE)

# Create custom driver age categories
baseFREQ$drv_age_cat <- cut(baseFREQ$agedriver,
                            breaks = c(18, 24, 34, 44, 64, 100),
                            labels = c("[18;24[", "[24;34[","[34;44[",
                                       "[44;64[","[64;101["),
                            right = TRUE,
                            include.lowest = TRUE)



### Correlation Heatmap of Numerical Features
numerical_vars <- baseFREQ %>%
  dplyr::select(nclaims, exposition, agedriver, agevehicle, power)

cor_matrix <- cor(numerical_vars, 
                  method = "spearman", 
                  use = "complete.obs")

corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "darkblue",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(100),
         mar = c(0,0,2,0),
         title = "Correlation Heatmap of baseFREQ variables",
         diag = FALSE)



### agedriver vs nclaims (size by exposition) scatterplot
ggplot(baseFREQ, aes(x = agedriver, y = nclaims, size = exposition, color = exposition)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Exposure (Policy-Years)") +
    scale_color_gradient(low = "blue", high = "red", name = "Exposure") +
    labs(title = "Scatterplot: Driver Age vs Claim Counts",
         x = "Driver Age", y = "Number of Claims",
         caption = "Size and color reflect exposure; sparse claims at higher ages") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### agevehicle vs nclaims (size by exposition) scatterplot
ggplot(baseFREQ, aes(x = agevehicle, y = nclaims, size = exposition, color = exposition)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Exposure (Policy-Years)") +
    scale_color_gradient(low = "blue", high = "red", name = "Exposure") +
    labs(title = "Scatterplot: Vehicle Age vs Claim Counts",
         x = "Vehicle Age", y = "Number of Claims",
         caption = "Size and color reflect exposure; older vehicles show varied claims") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### fuel vs nclaims (size by exposition) scatterplot
ggplot(baseFREQ, aes(x = fuel, y = nclaims, size = exposition, color = exposition)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Exposure (Policy-Years)") +
    scale_color_gradient(low = "blue", high = "red", name = "Exposure") +
    labs(title = "Scatterplot: Fuel vs Claim Counts",
         x = "Fuel", y = "Number of Claims",
         caption = "Size and color reflect exposure; older vehicles show varied claims") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### zone vs nclaims (size by exposition) scatterplot
ggplot(baseFREQ, aes(x = zone, y = nclaims, size = exposition, color = exposition)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Exposure (Policy-Years)") +
    scale_color_gradient(low = "blue", high = "red", name = "Exposure") +
    labs(title = "Scatterplot: Zone vs Claim Counts",
         x = "Zone", y = "Number of Claims",
         caption = "Size and color reflect exposure; older vehicles show varied claims") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### power vs nclaims (size by exposition) scatterplot
ggplot(baseFREQ, aes(x = power, y = nclaims, size = exposition, color = exposition)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Exposure (Policy-Years)") +
    scale_color_gradient(low = "blue", high = "red", name = "Exposure") +
    labs(title = "Scatterplot: Power vs Claim Counts",
         x = "Power", y = "Number of Claims",
         caption = "Size and color reflect exposure; older vehicles show varied claims") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### brand vs nclaims (size by exposition) scatterplot
ggplot(baseFREQ, aes(x = brand, y = nclaims, size = exposition, color = exposition)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(1, 10), name = "Exposure (Policy-Years)") +
    scale_color_gradient(low = "blue", high = "red", name = "Exposure") +
    labs(title = "Scatterplot: Brand vs Claim Counts",
         x = "Brand", y = "Number of Claims",
         caption = "Size and color reflect exposure; older vehicles show varied claims") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### nclaims by zone paired Boxplot
ggplot(baseFREQ, aes(x = zone, y = nclaims, fill = zone)) +
    geom_boxplot() +
    labs(title = "Boxplot: Claim Counts by Zone",
         x = "Zone", y = "Number of Claims",) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))


### agevehicle by fuel paired Boxplot
ggplot(baseFREQ, aes(x = fuel, y = agevehicle, fill = fuel)) +
    geom_boxplot() +
    labs(title = "Boxplot: Vehicle Age by Fuel Type",
         x = "Fuel Type", y = "Vehicle Age") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### agevehicle by drv_age_cat paired Boxplot
ggplot(baseFREQ, aes(x = drv_age_cat, y = agevehicle, fill = drv_age_cat)) +
    geom_boxplot() +
    labs(title = "Boxplot: Vehicle Age by Driver Age",
         x = "Driver Age", y = "Vehicle Age") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


### nclaims by drv_age_cat paired Boxplot
ggplot(baseFREQ, aes(x = drv_age_cat, y = nclaims, fill = drv_age_cat)) +
    geom_boxplot() +
    labs(title = "Boxplot: Claim Counts by Driver Age",
         x = "Driver Age", y = "Number of Claims",) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))



# Function for heatmap visualization
bivariate_analysis <- function(data, var1, var2, 
                               print_tables = TRUE, 
                               plot_heatmap = TRUE) {
    # Validate inputs
    if (!var1 %in% names(data)) stop(paste("Variable", var1, "not found in dataset"))
    if (!var2 %in% names(data)) stop(paste("Variable", var2, "not found in dataset"))
    
    # Convert to factors if not already
    data$var1_factor <- if (is.factor(data[[var1]])) {
        data[[var1]]
    } else {
        factor(data[[var1]])
    }
    
    data$var2_factor <- if (is.factor(data[[var2]])) {
        data[[var2]]
    } else {
        factor(data[[var2]])
    }
    
    # Calculate metrics
    policy_counts <- table(data$var1_factor, data$var2_factor)
    
    exposure <- tapply(data$exposition, 
                       list(data$var1_factor, data$var2_factor), 
                       sum, na.rm = TRUE)
    exposure[is.na(exposure)] <- 0
    
    claims <- tapply(data$nclaims, 
                     list(data$var1_factor, data$var2_factor), 
                     sum, na.rm = TRUE)
    claims[is.na(claims)] <- 0
    
    frequency <- claims / exposure
    frequency[exposure == 0] <- NA
    
    # Create formatted tables for output
    formatted_policy <- as.data.frame.matrix(policy_counts)
    formatted_exposure <- as.data.frame.matrix(round(exposure, 1))
    formatted_claims <- as.data.frame.matrix(claims)
    formatted_frequency <- as.data.frame.matrix(round(frequency, 4))
    
    # Create output list
    results <- list(
        var1_levels = levels(data$var1_factor),
        var2_levels = levels(data$var2_factor),
        policy_counts = policy_counts,
        exposure = exposure,
        claims = claims,
        frequency = frequency,
        formatted_tables = list(
            policies = formatted_policy,
            exposure = formatted_exposure,
            claims = formatted_claims,
            frequency = formatted_frequency
        )
    )
    
    # Print tables if requested
    if (print_tables) {
        cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
        cat(sprintf("BIVARIATE ANALYSIS: %s vs %s\n", var1, var2))
        cat(paste(rep("=", 70), collapse = ""), "\n")
        
        cat("\n", paste(rep("-", 70), collapse = ""), "\n")
        cat("NUMBER OF POLICIES:\n")
        print(formatted_policy)
        
        cat("\n\n", paste(rep("-", 70), collapse = ""), "\n")
        cat("TOTAL EXPOSURE (POLICY-YEARS):\n")
        print(formatted_exposure)
        
        cat("\n\n", paste(rep("-", 70), collapse = ""), "\n")
        cat("TOTAL CLAIMS:\n")
        print(formatted_claims)
        
        cat("\n\n", paste(rep("-", 70), collapse = ""), "\n")
        cat("CLAIM FREQUENCY (CLAIMS/POLICY-YEAR):\n")
        print(formatted_frequency)
        
        cat("\n", paste(rep("=", 70), collapse = ""), "\n\n")
    }
    
    # Create heatmap if requested
    if (plot_heatmap) {
        heatmap_data <- expand.grid(
            Var1 = results$var1_levels,
            Var2 = results$var2_levels
        )
        heatmap_data$Frequency <- as.vector(frequency)
        
        p <- ggplot(heatmap_data, aes(x = Var2, y = Var1, fill = Frequency)) +
            geom_tile(color = "white", linewidth = 0.8) +
            geom_text(aes(label = ifelse(is.na(Frequency), "N/A", 
                                         sprintf("%.4f", Frequency))), 
                      color = "black", size = 3.5, fontface = "bold") +
            scale_fill_gradientn(
                colors = c("#2b8cbe", "#a6bddb", "#fdbb84", "#e34a33"),
                na.value = "grey90",
                name = "Claim Frequency",
                limits = c(0, max(frequency, na.rm = TRUE) * 1.1)
            ) +
            labs(
                title = paste("Claim Frequency by", var1, "and", var2),
                subtitle = "Heatmap of Bivariate Interaction",
                x = var2,
                y = var1,
                caption = "Note: Frequency calculated as claims per policy-year"
            ) +
            theme_minimal(base_size = 12) +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = element_text(size = 10),
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                panel.grid = element_blank(),
                legend.position = "right",
                plot.caption = element_text(face = "italic")
            ) +
            coord_equal()
        
        results$heatmap <- p
        print(p)
    }
    
    return(invisible(results))
}

### Claim Frequency VS Zone & Driver Age
bivariate_analysis(data = baseFREQ, var1 = "zone",var2 = "drv_age_cat")


### Claim Frequency VS Driver Age & Vehicle Age
bivariate_analysis(data = baseFREQ, var1 = "drv_age_cat", var2 = "veh_age_cat")


### Claim Frequency VS Zone & Power
bivariate_analysis(data = baseFREQ, var1 = "zone", var2 = "power")


### Claim Frequency VS Brand & Power
bivariate_analysis(data = baseFREQ, var1 = "brand",var2 = "power")


### Claim Frequency VS Brand & Vehicle Age
bivariate_analysis(data = baseFREQ, var1 = "veh_age_cat",var2 = "brand")


### Claim Frequency VS Fuel & Vehicle Age
bivariate_analysis(data = baseFREQ, var1 = "fuel",var2 = "veh_age_cat")



# Function that constructs a graphic with claim frequency over risk levels
# Includes estimate through confidence interval for claim frequency
interactiongraphic <- function(title = "Claim Frequency vs Age of the Driver", 
                               name = "agedriver", 
                               lev = c(18, 24, 34, 44, 64, 100),
                               contin = TRUE) {
    
    if (contin == TRUE) {
        X <- cut(baseFREQ[, name], breaks = lev, include.lowest = TRUE)
    } else {
        X <- as.factor(baseFREQ[, name])
    }
    E <- baseFREQ$exposition
    Y <- baseFREQ$nclaims
    FREQ <- levels(X)
    mea <- variance <- n <- rep(NA, length(FREQ))
    
    for (k in 1:length(FREQ)) {
        mea[k] <- weighted.mean(Y[X == FREQ[k]] / E[X == FREQ[k]], E[X == FREQ[k]])
        variance[k] <- weighted.mean((Y[X == FREQ[k]] / E[X == FREQ[k]] - mea[k])^2, E[X == FREQ[k]])
        n[k] <- sum(E[X == FREQ[k]])
    }
    
    w <- barplot(n, names.arg = FREQ, col = "light blue", axes = FALSE, xlim = c(0, 1.2 * length(FREQ) + 0.5))
    mid <- w[, 1]
    axis(2)
    par(new = TRUE)
    IC1 <- mea + 1.96 * sqrt(variance) / sqrt(n)
    IC2 <- mea - 1.96 * sqrt(variance) / sqrt(n)
    globalmean <- sum(Y) / sum(E)
    
    plot(mid, mea, main = title, ylim = range(c(IC1, IC2)), type = "b", col = "red", 
         axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1.2 * length(FREQ) + 0.5))
    segments(mid, IC1, mid, IC2, col = "red")
    segments(mid - 0.1, IC1, mid + 0.1, IC1, col = "red")
    segments(mid - 0.1, IC2, mid + 0.1, IC2, col = "red")
    points(mid, mea, pch = 19, col = "red")
    axis(4)
    abline(h = globalmean, lty = 2, col = "red")
    
    mtext("Exposition", 2, line = 2, cex = 1.2, col = "light blue")
    mtext("Annual Frequency", 4, line = -2, cex = 1.2, col = "red")
}


# Age of the Driver
interactiongraphic()

# Zone of Residence 
interactiongraphic(title="Claim Frequency vs Zone of Residence",name="zone",contin=FALSE)

# Power of Vehicle
interactiongraphic(title="Claim Frequency vs Power of Vehicle",name="power",contin=FALSE)

# Age of Vehicle
interactiongraphic(title="Claim Frequency vs Age of Vehicle",name="agevehicle", contin=FALSE)

# Brand of Vehicle
interactiongraphic(title="Claim Frequency vs Brand of Vehicle",name="brand", contin=FALSE)

# Type of Fuel
interactiongraphic(title="Claim Frequency vs Type of Fuel",name="fuel", contin=FALSE)

######### Claim Costs Analysis #########

# Functions

bivariate_severity <- function(data, var1, var2,
                               print_tables = TRUE,
                               plot_heatmap = TRUE) {
  if (!var1 %in% names(data)) stop(paste("Variable", var1, "not found"))
  if (!var2 %in% names(data)) stop(paste("Variable", var2, "not found"))
  
  data$var1_factor <- factor(data[[var1]])
  data$var2_factor <- factor(data[[var2]])
  
  n_claims <- table(data$var1_factor, data$var2_factor)
  total_cost <- tapply(data$cost, list(data$var1_factor, data$var2_factor), sum, na.rm = TRUE)
  avg_cost <- tapply(data$cost, list(data$var1_factor, data$var2_factor), mean, na.rm = TRUE)
  avg_cost[is.nan(avg_cost)] <- NA
  
  formatted_n <- as.data.frame.matrix(n_claims)
  formatted_avg_cost <- as.data.frame.matrix(round(avg_cost, 2))
  
  results <- list(
    var1_levels = levels(data$var1_factor),
    var2_levels = levels(data$var2_factor),
    n_claims = n_claims,
    total_cost = total_cost,
    avg_cost = avg_cost,
    formatted_tables = list(
      n_claims = formatted_n,
      avg_cost = formatted_avg_cost
    )
  )
  
  if (print_tables) {
    cat("\n", strrep("=", 70), "\n")
    cat(sprintf("BIVARIATE SEVERITY ANALYSIS: %s vs %s\n", var1, var2))
    cat(strrep("=", 70), "\n\n")
    cat("NUMBER OF CLAIMS:\n")
    print(formatted_n)
    cat("\nAVERAGE CLAIM COST:\n")
    print(formatted_avg_cost)
    cat("\n", strrep("=", 70), "\n\n")
  }
  
  if (plot_heatmap) {
    heatmap_data <- expand.grid(
      Var1 = results$var1_levels,
      Var2 = results$var2_levels
    )
    heatmap_data$AvgCost <- as.vector(avg_cost)
    
    p <- ggplot(heatmap_data, aes(x = Var2, y = Var1, fill = AvgCost)) +
      geom_tile(color = "white", linewidth = 0.8) +
      geom_text(aes(label = ifelse(is.na(AvgCost), "N/A", sprintf("%.0f", AvgCost))),
                color = "black", size = 3.5, fontface = "bold") +
      scale_fill_gradientn(
        colors = c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"),  # NEW color scale
        na.value = "grey90",
        name = "Avg Cost (€)",
        limits = c(0, max(avg_cost, na.rm = TRUE) * 1.1)
      ) +
      labs(
        title = paste("Average Claim Cost by", var1, "and", var2),
        subtitle = "Heatmap of Bivariate Severity",
        x = var2,
        y = var1,
        caption = "Each tile shows mean claim cost"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right"
      ) +
      coord_equal()
    
    results$heatmap <- p
    print(p)
  }
  
  return(invisible(results))
}

# Bivariate Analysis
vars <- c("zone", "power", "brand", "fuel", "age_bin", "veh_age_bin")
pairs <- combn(vars, 2, simplify = FALSE)
for (pair in pairs) {
  cat("\n\n", strrep("=", 80), "\n")
  cat( pair[1], "vs", pair[2], "\n")
  cat(strrep("=", 80), "\n\n")
  
  try({
    bivariate_severity(baseSEV, var1 = pair[1], var2 = pair[2])
  }, silent = TRUE)
}

# Driver Age
Q <- quantile(baseSEV$agedriver, probs = (0:10)/10, na.rm = TRUE)
Q[1] <- Q[1] - 1
Q <- unique(Q) # debug
interactiongraphic_severity(name = "agedriver", title="Claim Severity vs Driver age", lev = Q, contin = TRUE)

bins <- c(18, 24, 34, 44, 64, 100)
interactiongraphic_severity(name = "agedriver", title="Claim Severity vs Driver age", lev = bins, contin = TRUE)

# Vehicle Age
Q_veh <- quantile(baseSEV$ageveh, probs = (0:10)/10, na.rm = TRUE)
Q_veh[1] <- Q_veh[1] - 1
Q_veh <- unique(Q_veh)
interactiongraphic_severity(name = "agevehicle", title="Claim Severity vs Vehicle age", lev = Q_veh, contin = TRUE)

veh_bins <- c(0, 2, 5, 10, 15, 25, 100)
interactiongraphic_severity(name = "agevehicle",title="Claim Severity vs Vehicle age", lev = veh_bins, contin = TRUE)

# Zone
interactiongraphic_severity(name = "zone", title="Claim Severity vs Zone", contin = FALSE)

# Power
interactiongraphic_severity(name = "power", title="Claim Severity vs Power", contin = FALSE)

# Brand
interactiongraphic_severity(name = "brand", title="Claim Severity vs Brand", contin = FALSE)

# Fuel
interactiongraphic_severity(name = "fuel", title="Claim Severity vs Fuel", contin = FALSE)

# ------------------------------------------------------------------------
#                 4. Distribution Fitting for Claims Data
# ------------------------------------------------------------------------

# Remove the highest outlier for the Number of Claims
length(baseFREQ$nclaims)
max(baseFREQ$nclaims)

baseFREQ.clean <- subset(baseFREQ, subset=nclaims<max(baseFREQ$nclaims))
length(baseFREQ.clean$nclaims)
max(baseFREQ.clean$nclaims)

# New Number of Claims Histogram
hist(baseFREQ.clean$nclaims,
     main = paste("New Distribution of Number of Claims"),
     xlab = "N claims",
     ylab = "Frequency", 
     col = 'blue')


# Test for a Negative Binomial Distribution
library(MASS);library(vcd)
model.nb <- goodfit(baseFREQ.clean$nclaims, type="nbinomial", method="ML")
model.nb
summary(model.nb) # we should not reject the hypothesis that data follows a Negative Binomial distribution

# Fit a Negative Binomial Distribution
library(fitdistrplus)
fit.nb <- fitdist(baseFREQ.clean$nclaims, "nbinom")

# Plots
plot(model.nb, main="Fitting a Negative Binomial distribution", xlab="Number of claims", ylab="Sqrt(Frequency)")
plot(fit.nb)



# Define upper threshold for outliers in Claim Severity
library(goft)
gamma_test(baseSEV$cost) # below 0.05 we reject that it follows a gamma distribution at the 5% significance level

# Finding the upper limit that maximizes the goodness of the gamma fit (largest p-value)
thresholds <- quantile(baseSEV$cost, probs = seq(0.90, 0.99, by = 0.005)) #0.5% increments
pvals <- sapply(thresholds, function(thr) {
  gamma_test(baseSEV$cost[baseSEV$cost <= thr])$p.value
})
par(mfrow = c(1, 1))
plot(thresholds, pvals, type = 'b', xlab = "Threshold", ylab = "Gamma p-value")

# Largest p-value
pvals[which.max(pvals)]
upper_lim <- thresholds[which.max(pvals)] # 96% percentile

# Remove defined large claims
baseSEV_withlim <- baseSEV[baseSEV$cost<=upper_lim,]
nrow(baseSEV)-nrow(baseSEV_withlim) # removed 77 large claims from data

# Re-test the gamma distribution for the data
gamma_test(baseSEV_withlim$cost) # at the 5% significance level, we can't reject that the data follows a gamma distribution
descdist(baseSEV_withlim$cost)

# Boxplot
boxplot(baseSEV_withlim$cost, main="Claim Costs BoxPlot",horizontal=TRUE, col="dodgerblue")

# Fit Gamma Distribution & Plot
fit.gamma <- fitdist(baseSEV_withlim$cost, "gamma")
fit.gamma
plot(fit.gamma)



# Removed Large claims
baseSEV_large_claims <- baseSEV[baseSEV$cost>=upper_lim,]

# Mean and Standard Deviaton
mean(baseSEV_large_claims$cost)
sd(baseSEV_large_claims$cost)

# Histogram and Boxplot 
par(mfrow = c(1, 2))
hist(baseSEV_large_claims$cost, main="Histogram of Large Claims", xlab='Claim Amount', ylab='Frequency', col='skyblue')
boxplot(baseSEV_large_claims$cost, horizontal=TRUE, main="Boxplot of Large Claims", col='lightgreen')

# ------------------------------------------------------------------------
#                                 Part II
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
#                 1. Modeling Claim Frequency with GLM
# ------------------------------------------------------------------------

# Updating age / vehicle age bin decisions on cleaned dataset
age_bins <- c(18, 24, 34, 44, 64, 100)
baseFREQ.clean$age_bin <- cut(baseFREQ.clean$agedriver, breaks=age_bins, right=FALSE)

vehicle_age_bins <- c(0, 2, 5, 10, 15, 25, 100)
baseFREQ.clean$veh_age_bin <- cut(baseFREQ.clean$agevehicle, breaks=vehicle_age_bins, right=FALSE)

# Checking the correlation between the driver's age and the age of the vehicle

cor(baseFREQ.clean$agedriver, baseFREQ.clean$agevehicle)  # it's -0.05 so we can include both

baseFREQ.clean$zone <- as.factor(baseFREQ.clean$zone)
baseFREQ.clean$brand <- as.factor(baseFREQ.clean$brand)
baseFREQ.clean$power <- as.factor(baseFREQ.clean$power)

# Fitting the model with the predictors and the log of exposition as offset

glm_nb_full <- glm.nb(nclaims ~ age_bin + zone + brand + veh_age_bin + power + fuel +
                            offset(log(exposition)),
                          data = baseFREQ.clean)
summary(glm_nb_full)


####### Age variable #######

# All age bins were already significant in the full model -> no need to try to group more


####### Vehicle Age variable #######

# No vehicle age bins were significant at the 95% level -> drop the entire feature


####### Zone variable #######

# Checking if we can group some levels
library(multcomp)
summary(glht(glm_nb_full,mcp(zone="Tukey"))) # We can group E and F together. Also, we might want to group A and B;

baseFREQ.clean$zone_grouped=baseFREQ.clean$zone
baseFREQ.clean$zone_grouped[baseFREQ.clean$zone %in% c("A", "B")] <- "A"
baseFREQ.clean$zone_grouped[baseFREQ.clean$zone %in% c("E", "F")] <- "E"
baseFREQ.clean$zone_grouped <- droplevels(as.factor(baseFREQ.clean$zone_grouped))

glm_nb_zone_grouped <- glm.nb(nclaims ~ age_bin + zone_grouped + brand + power + fuel +
                        offset(log(exposition)), 
                      data = baseFREQ.clean)
summary(glm_nb_zone_grouped)

anova(glm_nb_zone_grouped,glm_nb_full,test="Chisq") # p-value: 0.07 --> at the 95% level, we fail to reject the H0 that the simplified model is just as good

# Re-running the simultaneous tests for level differences
summary(glht(glm_nb_zone_grouped,mcp(zone_grouped="Tukey"))) # No more levels should be grouped


####### Brand variable #######

# Checking if we can group some levels

summary(glht(glm_nb_zone_grouped,mcp(brand="Tukey"))) 

# Lets start small (p-value=1) and go from there. 1-4-6-14; 3-5-10-11-13
baseFREQ.clean$brand_grouped=baseFREQ.clean$brand
baseFREQ.clean$brand_grouped[baseFREQ.clean$brand %in% c(1, 4, 6, 14)] <- 1
baseFREQ.clean$brand_grouped[baseFREQ.clean$brand %in% c(3, 5, 10, 11, 13)] <- 3
baseFREQ.clean$brand_grouped <- droplevels(as.factor(baseFREQ.clean$brand_grouped))

glm_nb_zone_brand_grouped <- glm.nb(nclaims ~ age_bin + zone_grouped + brand_grouped + power + fuel +
                                      offset(log(exposition)), 
                                    data = baseFREQ.clean)

summary(glm_nb_zone_brand_grouped)

anova(glm_nb_zone_brand_grouped, glm_nb_zone_grouped,test="Chisq") # p-value 0.94 so we fail to reject the null hypothesis that the models are different 

summary(glht(glm_nb_zone_brand_grouped,mcp(brand_grouped="Tukey"))) # Check again if any additional levels can be grouped -> brand 2 

# Grouping brand 2 with group 1-4-6-14
baseFREQ.clean$brand_grouped=baseFREQ.clean$brand
baseFREQ.clean$brand_grouped[baseFREQ.clean$brand %in% c(1, 2, 4, 6, 14)] <- 1
baseFREQ.clean$brand_grouped[baseFREQ.clean$brand %in% c(3, 5, 10, 11, 13)] <- 3
baseFREQ.clean$brand_grouped <- droplevels(as.factor(baseFREQ.clean$brand_grouped))

glm_nb_zone_brand_grouped2 <- glm.nb(nclaims ~ age_bin + zone_grouped + brand_grouped + power + fuel +
                                      offset(log(exposition)), 
                                    data = baseFREQ.clean)

summary(glm_nb_zone_brand_grouped2)

anova(glm_nb_zone_brand_grouped2, glm_nb_zone_grouped,test="Chisq") # p-value 0.85 so we fail to reject the null hypothesis that the models are different 

summary(glht(glm_nb_zone_brand_grouped2,mcp(brand_grouped="Tukey"))) # Check again if any additional levels can be grouped



####### Power variable #######

# Checking if we can group some levels

summary(glht(glm_nb_zone_brand_grouped2,mcp(power="Tukey")))  # There is no strong evidence that any particular power level differs from the others, except for power level 4 which deserves attention

baseFREQ.clean$power_grouped=baseFREQ.clean$power
baseFREQ.clean$power_grouped[baseFREQ.clean$power %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)] <- 5
baseFREQ.clean$power_grouped <- droplevels(as.factor(baseFREQ.clean$power_grouped))

glm_nb_zone_brand_power_grouped <- glm.nb(nclaims ~ age_bin + zone_grouped + brand_grouped + power_grouped + fuel +
                                       offset(log(exposition)), 
                                     data = baseFREQ.clean)

anova(glm_nb_zone_brand_grouped2, glm_nb_zone_brand_power_grouped,test="Chisq") # Fail to reject the models are significantly different

summary(glht(glm_nb_zone_brand_power_grouped,mcp(power_grouped="Tukey")))

summary(glm_nb_zone_brand_power_grouped)

####### Fuel variable #######

# Fuel is already statistically significant and binary


####### Final Model #######

model_final <- glm.nb(nclaims ~ age_bin + zone_grouped +
                        brand_grouped + power_grouped + fuel +
                        offset(log(exposition)),
                      data = baseFREQ.clean)
summary(model_final)

# Evaluating the Model's overall fit

# Deviance (Basically R^2 of GLMs) (sum of errors)

deviance(glm_nb_full)
deviance(model_final) # The final model fits the data better because it has lower deviance than the original model (12923.76 > 12922.62)

# AIC (error vs number of features/parameters)

AIC(glm_nb_full)
AIC(model_final) # The final model has lower AIC so it's better than the original one considering both fit and simplicity (20420.05 > 20397.42)

# Residuals

plot(residuals(glm_nb_full,type="deviance"),main="Residual Deviance", col="green")
hist(residuals(glm_nb_full,type="deviance"),main="Residual Deviance", col="green")

plot(residuals(model_final,type="deviance"),main="Residual Deviance", col="blue")
hist(residuals(model_final,type="deviance"),main="Residual Deviance", col="blue")


# Redefining the interaction plots using the new cleaned datasets
interactiongraphic_severity <- function(title = "Claim Severity vs Group",
                                        name = "agedriver",
                                        lev,
                                        contin = TRUE) {
  
  if (contin == TRUE) {
    # Continuous variable: bin using cut()
    X <- cut(as.numeric(baseSEV_withlim[[name]]), breaks = lev, include.lowest = TRUE) # Debug X <- cut(baseSEV_withlim[[name]], breaks = lev, include.lowest = TRUE)
  } else {
    # Categorical variable: convert to factor
    X <- as.factor(baseSEV_withlim[[name]])
  }
  
  Y <- baseSEV_withlim$cost
  FREQ <- levels(X)
  mea <- variance <- n <- rep(NA, length(FREQ))
  
  for (k in 1:length(FREQ)) {
    values <- Y[X == FREQ[k]]
    mea[k] <- mean(values, na.rm = TRUE)
    variance[k] <- var(values, na.rm = TRUE)
    n[k] <- sum(!is.na(values))
  }
  
  # Barplot of claim counts
  w <- barplot(n,
               names.arg = FREQ,
               col = "light blue",
               axes = FALSE,
               xlim = c(0, 1.2 * length(FREQ) + 0.5))
  mid <- w[, 1]
  axis(2)
  par(new = TRUE)
  
  # Confidence intervals
  IC1 <- mea + 1.96 * sqrt(variance / n)
  IC2 <- mea - 1.96 * sqrt(variance / n)
  globalmean <- mean(Y, na.rm = TRUE)
  
  # Line + CI plot
  plot(mid, mea,
       main = title,
       ylim = range(c(IC1, IC2), na.rm = TRUE),
       type = "b",
       col = "red",
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = c(0, 1.2 * length(FREQ) + 0.5))
  
  segments(mid, IC1, mid, IC2, col = "red")
  segments(mid - 0.1, IC1, mid + 0.1, IC1, col = "red")
  segments(mid - 0.1, IC2, mid + 0.1, IC2, col = "red")
  points(mid, mea, pch = 19, col = "red")
  axis(4)
  abline(h = globalmean, lty = 2, col = "red")
  
  # Labels
  mtext("Number of Claims", 2, line = 2, cex = 1.2, col = "light blue")
  mtext("Average Claim Cost", 4, line = -2, cex = 1.2, col = "red")
}

# Interaction graphic on final data to help choose standard insurer
interactiongraphic=function(title="Claim Frequency vs Age of the Driver",name="agedriver", lev,
                            contin=TRUE){
  
  if(contin==TRUE){X=cut(baseFREQ.clean[,name],lev)}
  if(contin==FALSE){X=as.factor(baseFREQ.clean[,name])}
  E=baseFREQ.clean$exposition
  Y=baseFREQ.clean$nclaims
  FREQ=levels(X)
  mea=variance=n=rep(NA,length(FREQ))
  for(k in 1:length(FREQ)){
    Ek = E[X == FREQ[k]]
    Yk = Y[X == FREQ[k]]
    if(sum(Ek, na.rm = TRUE) > 0){
      mea[k] = weighted.mean(Yk/Ek, Ek, na.rm = TRUE)
      variance[k] = weighted.mean((Yk/Ek - mea[k])^2, Ek, na.rm = TRUE)
      n[k] = sum(Ek, na.rm = TRUE)
    } else {
      mea[k] = NA
      variance[k] = NA
      n[k] = 0
    }
  }
  
  w=barplot(n,names.arg=FREQ,col="light blue", axes=FALSE,xlim=c(0,1.2*length(FREQ)+0.5))
  mid=w[,1]
  axis(2)
  par(new=TRUE)
  IC1=mea+1.96/sqrt(n)*sqrt(variance)
  IC2=mea-1.96/sqrt(n)*sqrt(variance)
  globalmean=sum(Y)/sum(E)
  
  plot(mid,mea,main=title,ylim=range(c(IC1,IC2)),type="b",col="red",axes=FALSE,xlab="",ylab="",xlim=c(0,1.2*length(FREQ)+0.5))
  segments(mid,IC1,mid,IC2,col="red")
  segments(mid-0.1,IC1,mid+0.1,IC1,col="red")
  segments(mid-0.1,IC2,mid+0.1,IC2,col="red")
  points(mid,mea,pch=19,col="red")
  axis(4)
  abline(h=globalmean,lty=2,col="red")
  
  mtext("Exposition",2,line=2,cex=1.2,col="light blue")
  mtext("Annual Frequency",4,line=-2,cex=1.2,col="red")
}

# Updating baseSEV_withlim to match baseFREQ.clean format 
baseSEV_withlim$zone_grouped=baseSEV_withlim$zone
baseSEV_withlim$zone_grouped[baseSEV_withlim$zone %in% c("A", "B")] <- "A"
baseSEV_withlim$zone_grouped[baseSEV_withlim$zone %in% c("E", "F")] <- "E"
baseSEV_withlim$zone_grouped <- droplevels(as.factor(baseSEV_withlim$zone_grouped))

baseSEV_withlim$brand_grouped=baseSEV_withlim$brand
baseSEV_withlim$brand_grouped[baseSEV_withlim$brand %in% c(1, 2, 4, 6, 14)] <- 1
baseSEV_withlim$brand_grouped[baseSEV_withlim$brand %in% c(3, 5, 10, 11, 13)] <- 3
baseSEV_withlim$brand_grouped <- droplevels(as.factor(baseSEV_withlim$brand_grouped))

baseSEV_withlim$power_grouped=baseSEV_withlim$power
baseSEV_withlim$power_grouped[baseSEV_withlim$power %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)] <- 5
baseSEV_withlim$power_grouped <- droplevels(as.factor(baseSEV_withlim$power_grouped))

baseSEV_withlim$age_bin <- cut(baseSEV_withlim$agedriver, breaks = c(18, 24, 34, 44, 64, 100), right = FALSE)

# Plots to help define the standard insurer - should have the shortest confidence interval / be the class with most statistical information

# Claim Frequency Plots
par(mfrow = c(2, 3))
interactiongraphic(title="Claim Frequency vs Zone", name="zone_grouped", contin=FALSE)
interactiongraphic(title="Claim Frequency vs Brand", name="brand_grouped", contin=FALSE)
interactiongraphic(title="Claim Frequency vs Power", name="power_grouped", contin=FALSE)
interactiongraphic(title="Claim Frequency vs Age", name="age_bin", contin=FALSE)
interactiongraphic(title="Claim Frequency vs Fuel", name="fuel", contin=FALSE)

# Claim Severity Plots
par(mfrow = c(2, 3))
interactiongraphic_severity(title = "Claim Severity vs Zone", name = "zone_grouped", contin = FALSE)
interactiongraphic_severity(title = "Claim Severity vs Brand", name = "brand_grouped", contin = FALSE)
interactiongraphic_severity(title = "Claim Severity vs Power", name = "power_grouped", contin = FALSE)
interactiongraphic_severity(title = "Claim Severity vs Age", name = "age_bin", contin = FALSE)
interactiongraphic_severity(title = "Claim Severity vs Fuel", name = "fuel", contin = FALSE)

# Standard Insurer: Zone A-B, Brand Group 1, Power group 5, Age [44,64[, Fuel E
# Define default levels
baseFREQ.clean$zone_grouped<-relevel(baseFREQ.clean$zone_grouped,"A")
baseFREQ.clean$brand_grouped<-relevel(baseFREQ.clean$brand_grouped,"1")
baseFREQ.clean$power_grouped<-relevel(baseFREQ.clean$power_grouped,"5")
baseFREQ.clean$age_bin<-relevel(baseFREQ.clean$age_bin,"[44,64)")
baseFREQ.clean$fuel<-relevel(as.factor(baseFREQ.clean$fuel),"E")


final_model <- glm.nb(nclaims ~ age_bin + zone_grouped +
                        brand_grouped + power_grouped + fuel +
                        offset(log(exposition)),
                      data = baseFREQ.clean)

summary(final_model)

# Identifying the Standard Insured profile and providing the corresponding claim frequency estimate.

# Standard Insurer, which is a driver with all baseline categories --> Intercept of the model

exp(coefficients(final_model)[1]) # the expected number of claims per year is 0.068, for someone with 1 year of exposure and all baseline risk characteristics


# Finding the insured profiles with the highest and lowest claim frequency risk and estimating the claim frequency for each one of them (1 year of exposure)

profiles <- expand.grid(
  age_bin = levels(baseFREQ.clean$age_bin),
  zone_grouped = levels(baseFREQ.clean$zone_grouped),
  brand_grouped = levels(baseFREQ.clean$brand_grouped),
  power_grouped = levels(baseFREQ.clean$power_grouped),
  fuel = levels(baseFREQ.clean$fuel),
  exposition = 1
)

# Predicting the claim frequency for every profile

profiles$predicted_claim_freq <- predict(final_model, newdata = profiles, type = "response")

# Highest risk profile
highest_risk <- profiles[which.max(profiles$predicted_claim_freq), ]
# Lowest risk profile
lowest_risk <- profiles[which.min(profiles$predicted_claim_freq), ]

print("Highest risk profile:")
print(highest_risk)
print("Lowest risk profile:")
print(lowest_risk)

# ------------------------------------------------------------------------
#           2. Modeling Claim Severity for Common Claims with GLM
# ------------------------------------------------------------------------

# Define default levels
baseSEV_withlim$zone_grouped<-relevel(baseSEV_withlim$zone_grouped,"A")
baseSEV_withlim$brand_grouped<-relevel(baseSEV_withlim$brand_grouped,"1")
baseSEV_withlim$power_grouped<-relevel(baseSEV_withlim$power_grouped,"5")
baseSEV_withlim$age_bin<-relevel(baseSEV_withlim$age_bin,"[44,64)")
baseSEV_withlim$fuel<-relevel(as.factor(baseSEV_withlim$fuel),"E")


# Gamma GLM
model_gamma <- glm(cost ~ zone_grouped + power_grouped + veh_age_bin + age_bin + brand_grouped + fuel,
                 family=Gamma(link="log"), # log (consistency with the nºclaims regression)
                 data=baseSEV_withlim)
summary(model_gamma)

####### Vehicle Age Variable #######

# No vehicle age group is statistically significant at the 95% level -> drop to match the Claim Frequency Model

model_gamma2 <- glm(cost ~ zone_grouped + power_grouped + age_bin + brand_grouped + fuel,
                    family=Gamma(link="log"), # log (consistency with the nºclaims regression)
                    data=baseSEV_withlim)

anova(model_gamma2, model_gamma,test="Chisq") # p-value: 0.51 so we failt to reject H0

summary(model_gamma2) # Happy with the model

####### Final Model ########
final_model_sev <- glm(cost ~ zone_grouped + power_grouped + age_bin + brand_grouped + fuel,
                    family=Gamma(link="log"), # log (consistency with the nºclaims regression)
                    data=baseSEV_withlim)

summary(final_model_sev)

# Evaluating the Model's overall fit

# Deviance (Basically R^2 of GLMs) (sum of errors) --> Deviance is slightly higher in the grouped model (1267.827 > 1262.746). This means we have a slightly worse fit but simpler structure and better interpretability

deviance(model_gamma)
deviance(final_model_sev)

# AIC (error vs number of features/parameters) --> AIC is lower in the grouped model (29535.71 > 29525.87). Even though the grouped model fits slightly worse in terms of raw deviance, the penalty for model complexity in the full model was too high.

AIC(model_gamma)
AIC(final_model_sev)

# Residuals
par(mfrow = c(1, 2))
plot(residuals(model_gamma,type="deviance"),main="Residual Deviance", col="green")
hist(residuals(model_gamma,type="deviance"),main="Residual Deviance", col="green")

plot(residuals(final_model_sev,type="deviance"),main="Residual Deviance", col="green")
hist(residuals(final_model_sev,type="deviance"),main="Residual Deviance", col="green")

# Standard Insurer, which is a driver with all baseline categories (same as above) --> Intercept of the model
exp(coefficients(final_model_sev)[1])

# Finding the insured profiles with the highest and lowest claim severity risk and estimating the claim frequency for each one of them (1 year of exposure)

# Predicting the claim frequency for every profile
profiles$predicted_claim_sev <- predict(final_model_sev, newdata = profiles, type = "response")

# Highest risk profile
highest_risk_sev <- profiles[which.max(profiles$predicted_claim_sev), ]
# Lowest risk profile
lowest_risk_sev <- profiles[which.min(profiles$predicted_claim_sev), ]

print("Highest risk profile:") # Cost = 1966.79
print(highest_risk_sev)
print("Lowest risk profile:") # Cost = 1041.43
print(lowest_risk_sev)

# ------------------------------------------------------------------------
#             3. Proposing a Pricing Structure for Common Claims
# ------------------------------------------------------------------------

# Frequency
freq_coefs <- summary(final_model)$coefficients
freq_coefs_df <- as.data.frame(freq_coefs)
write.csv(freq_coefs_df, "freq_coefs.csv", row.names = TRUE)
print(freq_coefs_df)


# Severity
sev_coefs <- summary(final_model_sev)$coefficients
sev_coefs_df <- as.data.frame(sev_coefs)
write.csv(sev_coefs_df, "sev_coefs.csv", row.names = TRUE)
print(sev_coefs_df)

# Pricing Structure for Common Claims derived in an Excel file

# ------------------------------------------------------------------------
#                 4. Modeling and Pricing for Large Claims
# ------------------------------------------------------------------------  

# Aligning baseSEV formatting with baseSEV_withlim and baseFREQ.clean
baseSEV$zone_grouped=baseSEV$zone
baseSEV$zone_grouped[baseSEV$zone %in% c("A", "B")] <- "A"
baseSEV$zone_grouped[baseSEV$zone %in% c("E", "F")] <- "E"
baseSEV$zone_grouped <- droplevels(as.factor(baseSEV$zone_grouped))

baseSEV$brand_grouped=baseSEV$brand
baseSEV$brand_grouped[baseSEV$brand %in% c(1, 2, 4, 6, 14)] <- 1
baseSEV$brand_grouped[baseSEV$brand %in% c(3, 5, 10, 11, 13)] <- 3
baseSEV$brand_grouped <- droplevels(as.factor(baseSEV$brand_grouped))

baseSEV$power_grouped=baseSEV$power
baseSEV$power_grouped[baseSEV$power %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)] <- 5
baseSEV$power_grouped <- droplevels(as.factor(baseSEV$power_grouped))

baseSEV$age_bin <- cut(baseSEV$agedriver, breaks = age_bins, right = FALSE)
baseSEV$veh_age_bin <- cut(baseSEV$agevehicle, breaks = vehicle_age_bins, right = FALSE)

# Define same default levels
baseSEV$zone_grouped<-relevel(baseSEV$zone_grouped,"A")
baseSEV$brand_grouped<-relevel(baseSEV$brand_grouped,"1")
baseSEV$power_grouped<-relevel(baseSEV$power_grouped,"5")
baseSEV$age_bin<-relevel(baseSEV$age_bin,"[44,64)")
baseSEV$fuel<-relevel(as.factor(baseSEV$fuel),"E")



# 1 - Fit a Logistic Regression to Large Claims to have as a benchmark

# Creating "large" variable for defining the reporting of a big claim
baseSEV$large_claim <- ifelse(baseSEV$cost >= upper_lim, 1, 0)

# Check class balance
table(baseSEV$large_claim)

# Fitting the Logistic Regression
reglogit_large_claim <-glm(large_claim ~ zone_grouped + power_grouped + veh_age_bin + age_bin + brand_grouped + fuel +
                             offset(log(exposition)),
                           data=baseSEV, family=binomial(link="logit"))
summary(reglogit_large_claim)

# Observing the odds Ratio (Ratio of positive probability (reporting large claim) over negative probability (not reporting large claim))
exp(coefficients(reglogit_large_claim))

logistic_preds <- predict(reglogit_large_claim, type="response")
logistic_true <- baseSEV$large_claim

# Create binary prediction
logistic_preds_class <- ifelse(logistic_preds >= 0.5, 1, 0)
confusionMatrix(as.factor(logistic_preds_class), as.factor(logistic_true), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data

library(MLmetrics)
F1_Score(y_pred = logistic_preds_class, y_true = logistic_true, positive = "1")

# Optimize threshold for confusion matrix
best_threshold_logit <- coords(roc_logit, "best", ret = "threshold", 
                               best.method = "youden")$threshold
logit_preds_class <- ifelse(logit_preds > best_threshold_logit, 1, 0)
confusionMatrix(as.factor(logit_preds_class), as.factor(logit_true), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data
F1_Score(y_pred = logit_preds_class, y_true = logit_true, positive = "1")



# 2 - Using a K-fold Cross Validation XGBoost ML model
library(pROC); library(caret)
set.seed(42)

# Number of folds
K <- 5

# Create stratified folds
stratified_folds <- function(y, K) {
  folds <- vector("list", K)
  pos_idx <- which(y == 1)
  neg_idx <- which(y == 0)
  
  pos_folds <- split(sample(pos_idx), rep(1:K, length.out = length(pos_idx)))
  neg_folds <- split(sample(neg_idx), rep(1:K, length.out = length(neg_idx)))
  
  for (k in 1:K) {
    folds[[k]] <- c(pos_folds[[k]], neg_folds[[k]])
  }
  return(folds)
}

folds <- stratified_folds(baseSEV$large_claim, K)


# Option 1
# All features without pre-processing
# XGBoost CV
library(xgboost)
baseSEV_xgb <- baseSEV

# Check class balance
table(baseSEV_xgb$large_claim)

xgb_auc <- c()
xgb_preds <- c()
xgb_true <- c()

# Set factor levels outside the loop
baseSEV_xgb$power <- as.factor(baseSEV_xgb$power)
baseSEV_xgb$brand <- as.factor(baseSEV_xgb$brand)

for (k in 1:K) {
  test_idx <- folds[[k]]
  train_idx <- setdiff(seq_len(nrow(baseSEV_xgb)), test_idx)
  
  train_data <- baseSEV_xgb[train_idx, ]
  test_data <- baseSEV_xgb[test_idx, ]
  
  # Design matrices (one-hot encode factors)
  train_matrix <- model.matrix(large_claim ~ zone+power+agevehicle+agedriver+brand+
                                 fuel -1, 
                               data = train_data)
  test_matrix <- model.matrix(large_claim ~ zone+power+agevehicle+agedriver+brand+
                                fuel -1, 
                              data = test_data)
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$large_claim, base_margin = log(train_data$exposition))
  dtest <- xgb.DMatrix(data = test_matrix, label = test_data$large_claim, base_margin = log(test_data$exposition))
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    scale_pos_weight = 1830 / 77  # Adjust for imbalance - penalize errors in minority class more heavily
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  probs <- predict(model, dtest)
  auc_k <- auc(test_data$large_claim, probs)
  
  xgb_auc <- c(xgb_auc, auc_k)
  xgb_preds <- c(xgb_preds, probs)
  xgb_true <- c(xgb_true, test_data$large_claim)
}

xgb_auc_mean <- mean(xgb_auc); xgb_auc_mean
roc_xgb <- roc(xgb_true, xgb_preds)
plot(roc_xgb, col = "darkgreen", main = "XGBoost ROC Curve")

# Create binary prediction
xgb_preds_class <- ifelse(xgb_preds > 0.5, 1, 0)
confusionMatrix(as.factor(xgb_preds_class), as.factor(xgb_true), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data

#install.packages("MLmetrics")
library(MLmetrics)
F1_Score(y_pred = xgb_preds_class, y_true = xgb_true, positive = "1")

# Optimize threshold for confusion matrix
best_threshold <- coords(roc_xgb, "best", ret = "threshold", 
                          best.method = "youden")$threshold
xgb_preds_class <- ifelse(xgb_preds > best_threshold, 1, 0)
confusionMatrix(as.factor(xgb_preds_class), as.factor(xgb_true), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data
F1_Score(y_pred = xgb_preds_class, y_true = xgb_true, positive = "1")

# feature importance
par(mfrow = c(1, 1))
importance <- xgb.importance(model = model)
xgb.plot.importance(importance)



# Option 2
# Top 2 features
xgb_auc2 <- c()
xgb_preds2 <- c()
xgb_true2 <- c()

for (k in 1:K) {
  test_idx <- folds[[k]]
  train_idx <- setdiff(seq_len(nrow(baseSEV_xgb)), test_idx)
  
  train_data <- baseSEV_xgb[train_idx, ]
  test_data <- baseSEV_xgb[test_idx, ]
  
  # Design matrices (one-hot encode factors)
  train_matrix <- model.matrix(large_claim ~ agevehicle + agedriver -1, 
                               data = train_data)
  test_matrix <- model.matrix(large_claim ~ agevehicle + agedriver -1, 
                              data = test_data)
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$large_claim, base_margin = log(train_data$exposition))
  dtest <- xgb.DMatrix(data = test_matrix, label = test_data$large_claim, base_margin = log(test_data$exposition))
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    scale_pos_weight = 1830 / 77  # Adjust for imbalance - penalize errors in minority class more heavily
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  probs <- predict(model, dtest)
  auc_k <- auc(test_data$large_claim, probs)
  
  xgb_auc2 <- c(xgb_auc2, auc_k)
  xgb_preds2 <- c(xgb_preds2, probs)
  xgb_true2 <- c(xgb_true2, test_data$large_claim)
}

xgb_auc_mean2 <- mean(xgb_auc); xgb_auc_mean2
roc_xgb2 <- roc(xgb_true2, xgb_preds2)
plot(roc_xgb2, col = "darkgreen", main = "XGBoost ROC Curve")

# Create binary prediction
xgb_preds_class2 <- ifelse(xgb_preds2 > 0.5, 1, 0)
confusionMatrix(as.factor(xgb_preds_class2), as.factor(xgb_true2), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data

F1_Score(y_pred = xgb_preds_class2, y_true = xgb_true2, positive = "1")

# Optimize threshold for confusion matrix
best_threshold2 <- coords(roc_xgb2, "best", ret = "threshold", 
                         best.method = "youden")$threshold
xgb_preds_class2 <- ifelse(xgb_preds2 > best_threshold2, 1, 0)
confusionMatrix(as.factor(xgb_preds_class2), as.factor(xgb_true2), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data
F1_Score(y_pred = xgb_preds_class2, y_true = xgb_true2, positive = "1")

# Feature importance
par(mfrow = c(1, 1))
importance <- xgb.importance(model = model)
xgb.plot.importance(importance)




# Option 3
# Using the same features as Common Claims GLMs
xgb_auc3 <- c()
xgb_preds3 <- c()
xgb_true3 <- c()

for (k in 1:K) {
  test_idx <- folds[[k]]
  train_idx <- setdiff(seq_len(nrow(baseSEV_xgb)), test_idx)
  
  train_data <- baseSEV_xgb[train_idx, ]
  test_data <- baseSEV_xgb[test_idx, ]
  
  # Design matrices (one-hot encode factors)
  train_matrix <- model.matrix(large_claim ~ zone_grouped + power_grouped + veh_age_bin + age_bin + brand_grouped + 
                                 fuel -1, 
                               data = train_data)
  test_matrix <- model.matrix(large_claim ~ zone_grouped + power_grouped + veh_age_bin + age_bin + brand_grouped +
                                fuel -1, 
                              data = test_data)
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = train_data$large_claim, base_margin = log(train_data$exposition))
  dtest <- xgb.DMatrix(data = test_matrix, label = test_data$large_claim, base_margin = log(test_data$exposition))
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    scale_pos_weight = 1830 / 77  # Adjust for imbalance - penalize errors in minority class more heavily
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  probs <- predict(model, dtest)
  auc_k <- auc(test_data$large_claim, probs)
  
  xgb_auc3 <- c(xgb_auc3, auc_k)
  xgb_preds3 <- c(xgb_preds3, probs)
  xgb_true3 <- c(xgb_true3, test_data$large_claim)
}

xgb_auc_mean3 <- mean(xgb_auc); xgb_auc_mean3
roc_xgb3 <- roc(xgb_true3, xgb_preds3)
plot(roc_xgb3, col = "darkgreen", main = "XGBoost ROC Curve")

# Create binary prediction
xgb_preds_class3 <- ifelse(xgb_preds3 > 0.5, 1, 0)
confusionMatrix(as.factor(xgb_preds_class3), as.factor(xgb_true3), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data

F1_Score(y_pred = xgb_preds_class3, y_true = xgb_true3, positive = "1")

# Optimize threshold for confusion matrix
best_threshold3 <- coords(roc_xgb3, "best", ret = "threshold", 
                         best.method = "youden")$threshold
xgb_preds_class3 <- ifelse(xgb_preds3 > best_threshold3, 1, 0)
confusionMatrix(as.factor(xgb_preds_class3), as.factor(xgb_true3), positive="1")  #illustrative purposes only — it's not too informative with imbalanced data
F1_Score(y_pred = xgb_preds_class3, y_true = xgb_true3, positive = "1")

# feature importance
par(mfrow = c(1, 1))
importance <- xgb.importance(model = model)
xgb.plot.importance(importance) # Can't remove any feature because features with bins of low importance have other bins with high importance


# Conclusion

# The second model with the best threshold applied shows the best 
# Balanced Accuracy, the best F1-Score, and is the simplest. 
# This should be the final model for large claims.



# Finding the expected value of a large claim

# Test for lognormal, gamma, weibull and pareto distributions
descdist(baseSEV_large_claims$cost)

# Fit log-normal, gamma, weibull, and pareto distributions
fit_lognorm <- fitdist(baseSEV_large_claims$cost, "lnorm")
fit_gamma   <- fitdist(baseSEV_large_claims$cost, "gamma")
fit_weibull <- fitdist(baseSEV_large_claims$cost, "weibull")

#install.packages("actuar")
library(actuar)
fit_pareto <- fitdist(baseSEV_large_claims$cost, "pareto", start = list(shape = 2, scale = 10000))

# Compare fits visually
cdfcomp(list(fit_lognorm, fit_gamma, fit_weibull, fit_pareto), legendtext = c("Lognormal", "Gamma", "Weibull", "Pareto"))
qqcomp(list(fit_lognorm, fit_gamma, fit_weibull, fit_pareto))

# Compare GOF statistics
gofstat(list(fit_lognorm, fit_gamma, fit_weibull, fit_pareto))

# All three goodness-of-fit statistics and criteria are lowest for Lognormal,
# indicating the best overall fit

# Expected value of a large claim
# meanlog and sdlog
fit_lognorm$estimate

# Expected value of a log-normal variable:
mu <- fit_lognorm$estimate["meanlog"]
sigma <- fit_lognorm$estimate["sdlog"]
expected_large_claim <- exp(mu + 0.5 * sigma^2)
names(expected_large_claim) <- 'Expected Value'; expected_large_claim
