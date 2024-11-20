#' Title- # Converting pounds to kilograms -
#'
#' @param pounds- A numeric value-
#'
#' @return a number or a float-
#' @export
#'
#' @examples
#' pounds_to_kg(75)-
pounds_to_kg <- function(pounds){
  kg <- pounds * 0.4536
  return(kg)
}

#' Title- Transforms age in years to age in months
#'
#' @param years- A numeric variable
#'
#' @return a number
#' @export
#'
#' @examples
#' years_to_months(3)
years_to_months <- function(years){
  month <- years * 12
  return(month)
}


#' Title- Converting Fahrenheit to Celsius.
#'
#' @param fahrenheit- A numeric variable
#'
#' @return a number
#' @export
#'
#' @examples
#' fahrenheit_to_celsius(97)
fahrenheit_to_celsius <- function(fahrenheit){
  celsius <- ((5/9) * (fahrenheit - 32))
  return(celsius)
}


#' Title- Converting Celsius to Fahrenheit.-
#'
#' @param celsius- A numeric or float variable
#'
#' @return a number
#' @export
#'
#' @examples
#' celsius_to_fahrenheit(37.5)
celsius_to_fahrenheit <- function(celsius){
  fahrenheit <- celsius*1.8+32
  return(fahrenheit)
}


#' Title: Calculate calories from macronutrients.
#'
#' @param carb_grams- a numeric or float variable
#' @param protein_grams- a numeric or float variable
#' @param fat_grams- a numeric or float variable
#'
#' @return a numeric variable or a float
#' @export
#'
#' @examples
#' calculate_calories(10, 5, 15)
#' calculate_calories(10, 5, 0)
calculate_calories <- function(carb_grams = 0, protein_grams=0, fat_grams = 0) {
  result <- (carb_grams * 4) + (protein_grams * 4) + (fat_grams * 9)
  return(result)
}



#' Title- Calculate bmi.
#'
#' @param weight- A numeric or a float variable
#' @param height- A numeric or a float variable
#'
#' @return bmi- A numeric or a float variable
#' @export
#'
#' @examples
#' calc_bmi(57, 172) or
#' medicaldata::smartpill |>
#'    as_tibble() |>
#'    select(Weight, Height) |>
#'    mutate(BMI = calc_bmi(Weight, Height))
calc_bmi <- function(weight=0, height=0){
  bmi <- weight/((height/100)^2)
  return(bmi)
}


#' Title- Converting celsius scale to other common scale.
#'
#' @param celsius- A numeric or a float variable
#' @param convert_to- The scale to convert to, e.g,fahrenheit, kelvin, centigrade. It must be a string.
#'
#' @return- A numeric or a float
#' @export
#'
#' @examples
#' celsius_convert(36.6, "fahrenheit")
celsius_convert <- function(celsius, convert_to){

  # Checking validity
  if (!convert_to %in% c("fahrenheit", "kelvin", "centigrade")){
    stop("convert to must be one of 'fahrenheit; or 'kelvin'")
  }

  # Converting value
  if (convert_to == "fahrenheit"){
    out <- (celsius * 9/5) + 32
  } else if (convert_to == "kelvin"){
    out <- celsius + 273.15
  } else if (convert_to == "centigrade"){
    out <- celsius
  }
  return(out)
}

#' Title: Calculate calories from macronutrients.
#'
#' @param carb_grams- a numeric or float variable
#' @param protein_grams- a numeric or float variable
#' @param fat_grams- a numeric or float variable
#'
#' @return a numeric variable or a float
#' @export
#'
#' @examples
#' calculate_calories(10, 5, 15)
#' calculate_calories(10, 5, 0)
calculate_calories2 <- function(carb_grams = 0, protein_grams = 0, fat_grams = 0) {

  # your code here
  if (!is.numeric(c(carb_grams, protein_grams, fat_grams))){
    stop("All arguments must be numeric")
  }
  result <- (carb_grams * 4) + (protein_grams * 4) + (fat_grams * 9)
  return(result)
}


#' Title- Classify body temperatue of humans.-
#'
#' @param temp- A numeric or a float variable
#'
#' @return - a string
#' @export
#'
#' @examples
#' classify_temp(36.9)
classify_temp <- function(temp){
  out <-  ifelse(temp < 35, "hypothermia",
                 ifelse(temp >= 35 & temp <= 37, "normal",
                        ifelse(temp > 37, "fever", "NA")))
  return(out)
}


#' Title- Classify body temperatue of humans.-
#'
#' @param temp- A numeric or a float variable
#'
#' @return - a string
#' @export
#'
#' @examples
#' classify_temp(36.9)
classify_temp2 <- function(temp) {
  if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)|>
  dplyr::case_when(
    temp < 35 ~ "hypothermia",
    temp >= 35 & temp <= 37 ~ "normal",
    temp > 37 ~ "fever",
    TRUE ~ NA_character_
  )
}


#' Title- Calculate isoniazide dosage.-
#'
#' @param weight- A numeric, a float, a vector or or a numeric variable in a DataFrame
#'
#' @return- A dosage, which must be numeric
#' @export
#'
#' @examples
#' calculate_isoniazid_dosage2(c(42, 30, 70)) or
#'
#' medicaldata::smartpill |>
#' as_tibble() |>
#'  select(Weight) |>
#'  mutate(dosage = calculate_isoniazid_dosage2(Weight))
calculate_isoniazid_dosage2 <- function(weight) {
  if (any(weight < 30)) stop("Weights must all be at least 30 kg.")

  # Your code here
  {
    out <- ifelse(weight <= 35, 150,
                  ifelse(weight <= 45, 200,
                         ifelse(weight <= 55, 300,
                                ifelse(weight <= 70, 300, 300))))
    return(out)

  }
  return(out)
}

# or

#' Title- Calculate isoniazide dosage.-
#'
#' @param weight- A numeric, a float, a vector or or a numeric variable in a DataFrame
#'
#' @return- A dosage, which must be numeric
#' @export
#'
#' @examples
#' calculate_isoniazid_dosage2(c(42, 30, 70)) or
#'
#' medicaldata::smartpill |>
#' as_tibble() |>
#'  select(Weight) |>
#'  mutate(dosage = calculate_isoniazid_dosage2(Weight))
calculate_isoniazid_dosage2 <- function(weight) {
  if (any(weight < 30)) stop("Weights must all be at least 30 kg.")

  dosage <- if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)|>
  dplyr::case_when(
    weight <= 35 ~ 150,
    weight <= 45 ~ 200,
    weight <= 55 ~ 300,
    weight <= 70 ~ 300,
    TRUE ~ 300
  )
  return(dosage)
}
