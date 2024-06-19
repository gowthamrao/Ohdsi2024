databasesOfInterest <-
  c("IBM CCAE",
    "IBM MDCD",
    "IBM MDCR",
    "Optum EHR",
    "OPTUM Extended SES",
    "PharMetrics") |> unique() |> sort()

ageGroupsOfInterest <-
  c("30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "All") #"18 - 29",

removeConditions <-
  c(
    "Scleroderma",
    "Left Heart Failure" ,
    "Ankylosing Spondylitis",
    "Sarcoidosis",
    "Coronary artery disease"
  )

rawData <-
  readr::read_csv(file = "D:\\studyResults\\HowOften\\result-data-full-incidenceRateTable-2024-06-17.csv", col_types = readr::cols()) |>
  dplyr::select(
    -refId,-databaseId,-sourceName,-subgroupId,-outcomeId,-cleanWindow,-ageId,-genderId,-personsAtRiskPe,-personDaysPe,-personOutcomesPe,-outcomesPe,-outcomeCohortDefinitionId,-outcomeIdShort,-targetIdShort
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = "Earliest event of ",
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = stringr::fixed(" indexed on signs, symptoms, treatment, or diagnosis (FP)"),
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = stringr::fixed(", with NO occurrence of certain psychiatric disorder"),
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = stringr::fixed(" including its complications"),
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = stringr::fixed(" disease derived from Imfeld, 2"),
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = "All events of ",
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = "Earliest Event of ",
      replacement = ""
    )
  ) |>
  dplyr::mutate(
    targetName = stringr::str_replace_all(
      string = targetName,
      pattern = "Persons with ",
      replacement = ""
    )
  ) |>
  dplyr::filter(
    cdmSourceAbbreviation %in% databasesOfInterest,
    ageGroupName %in% ageGroupsOfInterest
  ) |>
  dplyr::filter(stringr::str_detect(
    string = tolower(targetName),
    pattern = tolower(removeConditions) |> paste0(collapse = "|"),
    negate = TRUE
  ))

rawData <- rawData |>
  dplyr::mutate(
    targetName = OhdsiHelpers::capitalizeFirstLetter(targetName),
    outcomeName = OhdsiHelpers::capitalizeFirstLetter(outcomeName)
  )

rawData <- rawData |>
  dplyr::mutate(
    conditionGroup = dplyr::case_when(
      .data$targetName %in% c(
        "Crohns disease",
        "Plaque Psoriasis",
        "Psoriatic arthritis",
        "Rheumatoid Arthritis",
        "Systemic lupus erythematosus",
        "Ulcerative colitis"
      ) ~ "Immunology",
      .data$targetName %in% c(
        "Acute Myocardial Infarction",
        "Atrial Fibrillation or Flutter",
        "Coronary artery disease (CAD)",
        "Heart failure",
        "Hemorrhagic Stroke"
      ) ~ "Cardiovascular",
      .data$targetName %in% c(
        "Alzheimer's",
        "Epilepsy",
        "Major depressive disorder",
        "Multiple Sclerosis"
      ) ~ "Neuroscience",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::relocate(conditionGroup) |>
  dplyr::mutate(targetNameWithId = paste0(targetName, " (", (targetCohortDefinitionId -
                                                               1) / 1000, ")")) |>
  dplyr::mutate(incidenceProportion = incidenceProportionP100p / 100)

# table 1: persons studied
rawData |>
  dplyr::filter(ageGroupName != 'All') |>
  dplyr::group_by(conditionGroup,
                  targetName) |>
  dplyr::summarise(count = sum(personOutcomes), .groups = "keep") |>
  dplyr::ungroup() |>
  dplyr::rename(disease = targetName) |>
  dplyr::mutate(count = OhdsiHelpers::formatIntegerWithComma(count)) |>
  dplyr::arrange(conditionGroup, disease) |>
  clipr::write_clip(object_type = "table")

# create a heat map of incidence rate for each target grouped by data source
plotData <- rawData |>
  dplyr::filter(
    startYear != 'All',
    ageGroupName != 'All',
    # cdmSourceAbbreviation %in% c("OPTUM Extended SES"),
    ageGroupName %in% ageGroupsOfInterest,
    # targetName == "Acute Myocardial Infarction",
    outcomeName == 'Inpatient Hospitalization (0Pe, 1Era)',
    genderName != 'All'
  ) |>
  dplyr::mutate(startYear = as.numeric(startYear)) |>
  dplyr::group_by(# genderName,
    #               startYear,
    conditionGroup,
    cdmSourceAbbreviation,
    targetName) |>
  dplyr::summarise(
    personDays = sum(personDays),
    personOutcomes = sum(personOutcomes),
    outcomes = sum(outcomes),
    personsAtRisk = sum(personsAtRisk),
    .groups = "keep"
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    incidenceRateP100py = (outcomes / (personDays / 365.25)) * 100,
    incidenceProportion = (personOutcomes / personsAtRisk) *
      100
  ) |>
  dplyr::select(-outcomes, -personDays, -personOutcomes, -personsAtRisk) |>
  tidyr::pivot_wider(
    id_cols = c(conditionGroup,
                targetName),
    values_from =  incidenceProportion,
    names_from = cdmSourceAbbreviation
  ) |>
  dplyr::arrange(conditionGroup, targetName) |>
  clipr::write_clip(object_type = "table")



# plot of incidenceProportion for each gender across all years, by database and
plotData <- rawData |>
  dplyr::filter(
    startYear != 'All',
    cdmSourceAbbreviation %in% databasesOfInterest,
    ageGroupName %in% ageGroupsOfInterest
  ) |>
  dplyr::group_by(
    startYear,
    genderName,
    ageGroupName,
    conditionGroup,
    targetName,
    cdmSourceAbbreviation
  ) |>
  dplyr::summarise(
    personDays = sum(personDays),
    personOutcomes = sum(personOutcomes),
    outcomes = sum(outcomes),
    personsAtRisk = sum(personsAtRisk),
    .groups = "keep"
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    incidenceRateP100py = (outcomes / (personDays / 365.25)) * 100,
    incidenceProportion = (personOutcomes / personsAtRisk) *
      100
  ) |>
  dplyr::select(-outcomes,
                -personDays,
                -personOutcomes,
                -personsAtRisk,
                -incidenceProportion) |>
  dplyr::filter(genderName %in% c("MALE", "FEMALE"),
                ageGroupName != 'All') |>
  dplyr::filter(cdmSourceAbbreviation == "OPTUM Extended SES")



createPlot <-
  function(df,
           titleText,
           yAxisLabel,
           smoothMethod = "loess",
           smoothSpan = 0.75,
           colorPalette = c(OhdsiRPlots::createOhdsiPalette()[[1]],
                            OhdsiRPlots::createOhdsiPalette()[[5]])) {
    # Error handling: Check if necessary columns exist
    requiredCols <-
      c("startYear",
        "incidenceRateP100py",
        "genderName",
        "ageGroupName")
    missingCols <- setdiff(requiredCols, names(df))
    if (length(missingCols) > 0) {
      stop(paste(
        "Missing columns in data frame:",
        paste(missingCols, collapse = ", ")
      ))
    }
    
    library(ggplot2)
    ggplot(df, aes(
      x = startYear,
      y = incidenceRateP100py,
      color = genderName,        # Color by gender
      group = genderName         # Group by gender for smoothing
    )) +
      geom_point() +
      {
        if (!is.null(smoothMethod)) {
          geom_smooth(
            method = smoothMethod,
            se = FALSE,
            span = smoothSpan,
            linetype = "dashed",   # Set a default linetype (you can customize)
            size = 0.5,
            alpha = 0.1
          )
        }
      } +
      facet_grid(~ageGroupName, switch = "x") +
      
      # Combined Color and Linetype Scale
      scale_color_manual(
        values = colorPalette,
        name = "Gender",
        guide = guide_legend(override.aes = list(linetype = c("solid", "dashed"))) 
      ) + 
      # Removed scale_shape_manual since we don't use it anymore
      
      # Combine color and linetype scale to a single legend
      scale_color_manual(values = colorPalette, name = "Gender") +
      scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +  # Associate line types with genders
      
      labs(x = "Year", y = yAxisLabel, title = titleText) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing.x = unit(1, "lines"),
        panel.background = element_rect(fill = "grey100", linetype = "dotted"),
        axis.ticks.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank()
      ) +
      scale_x_continuous(breaks = seq(min(df$startYear), max(df$startYear), by = 5)) +
      scale_y_continuous(limits = c(0, NA))
  }

conditionGroups <- plotData$conditionGroup |> unique() |> sort()


selectedConditionGroup <- conditionGroups[[1]]
targetNames <- plotData |> 
  dplyr::filter(conditionGroup == selectedConditionGroup) |> 
  dplyr::pull(targetName) |> unique() |> sort()

plots <- list()
for (j in (1:length(targetNames))) {
  plotDataFiltered <- plotData |>
    dplyr::filter(targetName == targetNames[[j]]) |> 
    dplyr::mutate(startYear = as.numeric(startYear))
  
  # Assuming your data is in a dataframe called 'df'
  plots[[j]] <- # Create the plot
    createPlot(df = plotDataFiltered, titleText = targetNames[[j]], yAxisLabel = "Incidence Rate Per 100 PY")
  
}

conditionGroup1 <- OhdsiRPlots::arrangeGgplots(
  ggplotList = plots,
  layoutInstruction = 'ncol = 2, nrow = 2',
  shareX = TRUE,
  shareY = TRUE
)


selectedConditionGroup <- conditionGroups[[2]]
targetNames <- plotData |> 
  dplyr::filter(conditionGroup == selectedConditionGroup) |> 
  dplyr::pull(targetName) |> unique() |> sort()

plots <- list()
for (j in (1:length(targetNames))) {
  plotDataFiltered <- plotData |>
    dplyr::filter(targetName == targetNames[[j]]) |> 
    dplyr::mutate(startYear = as.numeric(startYear))
  
  # Assuming your data is in a dataframe called 'df'
  plots[[j]] <- # Create the plot
    createPlot(df = plotDataFiltered, titleText = targetNames[[j]], yAxisLabel = "Incidence Rate Per 100 PY")
}

conditionGroup2 <- OhdsiRPlots::arrangeGgplots(
  ggplotList = plots,
  layoutInstruction = 'ncol = 2, nrow = 3',
  shareX = TRUE,
  shareY = TRUE
)



selectedConditionGroup <- conditionGroups[[3]]
targetNames <- plotData |> 
  dplyr::filter(conditionGroup == selectedConditionGroup) |> 
  dplyr::pull(targetName) |> unique() |> sort()

plots <- list()
for (j in (1:length(targetNames))) {
  plotDataFiltered <- plotData |>
    dplyr::filter(targetName == targetNames[[j]]) |> 
    dplyr::mutate(startYear = as.numeric(startYear))
  
  # Assuming your data is in a dataframe called 'df'
  plots[[j]] <- # Create the plot
    createPlot(df = plotDataFiltered, titleText = targetNames[[j]], yAxisLabel = "Incidence Rate Per 100 PY")
}

conditionGroup3 <- OhdsiRPlots::arrangeGgplots(
  ggplotList = plots,
  layoutInstruction = 'ncol = 2, nrow = 2',
  shareX = TRUE,
  shareY = TRUE
)


OhdsiRPlots::arrangeGgplots(
  ggplotList = list(conditionGroup1, conditionGroup2, conditionGroup3),
  layoutInstruction = 'ncol = 1, nrow = 3',
  shareX = TRUE,
  shareY = TRUE
)



##############

# plot of incidenceProportion * calendarYear -- all age, all gender
plotData <- rawData |>
  dplyr::filter(
    startYear != 'All',
    genderName != 'All',
    cdmSourceAbbreviation %in% databasesOfInterest,
    ageGroupName %in% ageGroupsOfInterest
  ) |>
  dplyr::group_by(
    startYear,
    conditionGroup,
    targetName,
    cdmSourceAbbreviation
  ) |>
  dplyr::summarise(
    personDays = sum(personDays),
    personOutcomes = sum(personOutcomes),
    outcomes = sum(outcomes),
    personsAtRisk = sum(personsAtRisk),
    .groups = "keep"
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    personYears = personDays/365.25,
    incidenceRateP100py = (outcomes / (personDays / 365.25)) * 100,
    incidenceProportion = (personOutcomes / personsAtRisk) *
      100
  ) |>
  dplyr::select(-personDays,
                -personOutcomes,
                -personsAtRisk,
                -incidenceProportion)


createPlot <- function(df) {
  
  df <- df |> 
    dplyr::mutate(startYear = as.numeric(startYear))
  library(ggplot2)
  library(OhdsiRPlots)
  library(patchwork)# For arranging plots
  
  selectedConditionGroup <- df$conditionGroup |> unique()
  
  # 1. Unique Target Names and Color Palette
  uniqueTargetNames <- unique(df$targetName)
  palette <- OhdsiRPlots::createOhdsiPalette(numColors = length(uniqueTargetNames))
  
  # 2. Color Mapping
  color_map <- setNames(palette, uniqueTargetNames)
  
  # 4. Create Individual Plots
  plots <- lapply(unique(df$cdmSourceAbbreviation), function(source) {
    filtered_df <- df[df$cdmSourceAbbreviation == source, ]
    
    plot <- ggplot(filtered_df, aes(x = startYear, y = incidenceRateP100py, group = targetName, color = targetName)) +
      geom_line(alpha = 0.5) +
      geom_smooth(se = FALSE) +
      labs(x = "Start Year", y = "IR Per 100 PY",
           title = source) +
      scale_color_manual(values = color_map) +
      theme_minimal() +
      expand_limits(y = 0) +
      scale_x_continuous(breaks = seq(min(filtered_df$startYear), max(filtered_df$startYear), by = 5)) +
      theme(legend.position = "none") # Remove legend from individual plots
    
    return(plot)
  })
  
  # Combine plots and add a shared legend
  combinedPlot <- wrap_plots(plots) + plot_layout(guides = 'collect')
  combinedPlot <- combinedPlot & theme(legend.position = "right") # Position the collected legend at the bottom
  
  return(combinedPlot)
}






conditionGroups <- plotData$conditionGroup |> unique() |> sort()

plots <- list()

plots[[1]] <- plotData |>
  dplyr::filter(conditionGroup == conditionGroups[[1]]) |>
  createPlot()
plots[[2]] <- plotData |>
  dplyr::filter(conditionGroup == conditionGroups[[2]]) |>
  createPlot()
plots[[3]] <- plotData |>
  dplyr::filter(conditionGroup == conditionGroups[[3]]) |>
  createPlot()


conditionGroup1 <- OhdsiRPlots::arrangeGgplots(
  ggplotList = plots,
  layoutInstruction = 'nrow = 3',
  shareX = TRUE,
  shareY = TRUE
)



plotDataFiltered <- plotData |> 
  dplyr::filter(targetName == 'Acute Myocardial Infarction',
                startYear == '2012') |> 
  dplyr::select(startYear,
                conditionGroup,
                targetName,
                cdmSourceAbbreviation,
                outcomes,
                personYears)

irRnd <- meta::metarate(
  data = plotDataFiltered,
  event = outcomes,
  time = personYears,
  method = "Inverse",
  studlab = cdmSourceAbbreviation,
  sm = "IRLN",
  random = TRUE,
  method.tau = "DL",
  irscale = 100000,
  irunit = "person-years",
  prediction = TRUE
)


library(meta)
library(dplyr)

# Filter the data
plotDataFiltered <- plotData |> 
  dplyr::filter(targetName == 'Acute Myocardial Infarction',
                startYear == '2012') |> 
  dplyr::select(startYear, conditionGroup, targetName, cdmSourceAbbreviation, outcomes, personYears, incidenceRateP100py)

# Define the function to extract meta-analysis results
rand_te <- function(data) {
  random.meta <- meta::metarate(
    data = data,
    event = outcomes,
    time = personYears,
    studlab = cdmSourceAbbreviation,
    sm = "IRLN",
    comb.random = TRUE,
    method.tau = "DL"
  )
  
  # Extract relevant statistics from the meta-analysis result
  random.te <- random.meta[["TE.random"]]
  random.te.lower <- random.meta[["lower.random"]]
  random.te.upper <- random.meta[["upper.random"]]
  seTE.random <- random.meta[["seTE.random"]]
  tau2 <- random.meta[["tau2"]]
  se.tau2 <- random.meta[["se.tau2"]]
  tau <- random.meta[["tau"]]
  lower.predict <- random.meta[["lower.predict"]]
  upper.predict <- random.meta[["upper.predict"]]
  seTE.predict <- random.meta[["seTE.predict"]]
  
  # Return a data frame with the meta-analysis results
  meta.out <-
    data.frame(
      random.te,
      random.te.lower,
      random.te.upper,
      seTE.random,
      tau2,
      se.tau2,
      tau,
      lower.predict,
      upper.predict,
      seTE.predict
    ) %>%
    dplyr::mutate(
      ir.rand = exp(random.te) * 100,
      ir.rand.l = exp(random.te.lower) * 100,
      ir.rand.u = exp(random.te.upper) * 100,
      ir.predict.lower = exp(lower.predict) * 100,
      ir.predict.upper = exp(upper.predict) * 100
    ) |>
    dplyr::tibble()
  return(meta.out)
}

meta <- rand_te(data = plotData)




