setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(require(pacman)==FALSE) install.packages("pacman")

pacman::p_load(tidyverse,
               magrittr, 
               lubridate,
               readxl,
               rvest)


# GDP Data ----------------------------------------------------------------
#from World Bank

gdp = read.csv("GDP Data.csv")

#pivoting the data
gdp %>% pivot_longer(cols = c("X1960":"X2020"), 
                     names_to = "year", 
                     values_to = "gdp per capita") -> gdp

#removing X from the year variable
gdp$year = str_remove(gdp$year, "X")

gdp %<>% select(Country.Name, Country.Code, year, `gdp per capita`)


# Death Rate Per 1,000 ----------------------------------------------------
#from World Bank

death = read.csv('Death Rate Per 1000.csv')

#pivoting the data
death %>% pivot_longer(cols = c("X1960":"X2020"), 
                     names_to = "year", 
                     values_to = "death rate per 1000") -> death

#removing X from the year variable
death$year = str_remove(death$year, "X")

death %<>% select(Country.Name, Country.Code, year, `death rate per 1000`)

#calculating general summary statistics
death2019 = death %>% filter(year == '2019' & Country.Name == 'World')
death1970 = death %>% filter(year == '1970' & Country.Name == 'World')

# Life Expectancy ---------------------------------------------------------
#from World Bank

letotal = read.csv('Life Expectancy Total.csv')

#pivoting the data
letotal %>% pivot_longer(cols = c("X1960":"X2020"), 
                         names_to = "year", 
                         values_to = "Life Expectancy Total") -> letotal

letotal %<>% select(Country.Name, Country.Code, year, `Life Expectancy Total`)

#removing X from the year variable
letotal$year = str_remove(letotal$year, "X")

#getting highest and lowest life expectancy in 2019
le2019 = letotal %>% filter(year == '2019')

#calculating general summary statistics
le2019world = letotal %>% filter(year == '2019' & Country.Name == 'World')
le1970world = letotal %>% filter(year == '1970' & Country.Name == 'World')

# Population Data ---------------------------------------------------------
#from World Bank

pop = read.csv('Population Data.csv')

#pivoting the data
pop %>% pivot_longer(cols = c("X1960":"X2020"), 
                     names_to = "year", 
                     values_to = "population") -> pop

#removing X from the year variable
pop$year = str_remove(pop$year, "X")

pop %<>% select(Country.Name, Country.Code, year, population)



# Unemployment ------------------------------------------------------------
#from World Bank

unemployment = read.csv('Unemployment.csv')

#pivoting the data
unemployment %>% pivot_longer(cols = c("X1960":"X2020"), 
                     names_to = "year", 
                     values_to = "unemployment_rate") -> unemployment

#removing X from the year variable
unemployment$year = str_remove(unemployment$year, "X")

unemployment %<>% select(Country.Name, Country.Code, year, unemployment_rate)


# Cause of Death ----------------------------------------------------------
#https://www.cdc.gov/nchs/data-visualization/mortality-leading-causes/

cause = read.csv('NCHS_-_Leading_Causes_of_Death__United_States.csv')

cause %<>% select(Year, State, Cause.Name, Deaths)

#removing comma from the total death numbers and then converting to a numeric
cause$Deaths %<>% str_remove(',')
cause$Deaths %<>% as.numeric()

#filtering out the states
cause %<>% filter(State=='United States')

#removing all causes from the data
cause %<>% filter(Cause.Name != "All causes") 

#pivoting the data to make each cause of death its own variable
cause %>%  pivot_wider(names_from = Cause.Name, 
                        values_from = Deaths) -> cause

cause$COVID19 = 0
colnames(cause)[1] = 'year'
colnames(cause)[2] = 'Country.Name'
colnames(cause)[4] = 'Alzheimers disease'

cause$year %<>% as.character()

cause %<>% select(-c(suicide))


# 2020 Cause of Death -----------------------------------------------------
#https://jamanetwork.com/journals/jama/fullarticle/2778234

covidcause = read_excel('2020 Cause of Death.xlsx')

covidcause$year %<>% as.character()

#joining the 2020 cause of death data to the rest of the data
cause = full_join(covidcause, cause, by = c('Country.Name', 
                                            'year',
                                            'Heart disease', 
                                            'Cancer', 
                                            'Alzheimers disease',
                                            'CLRD',
                                            'Kidney disease',
                                            'Diabetes',
                                            'Suicide',
                                            'Influenza and pneumonia',
                                            'Stroke',
                                            'Unintentional injuries',
                                            'COVID19'))



# Overdose Deaths ---------------------------------------------------------
#https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=D6E3ACDA49594B13E0AEED1B4BD4
#2020 Data point:
#https://www.cdc.gov/nchs/nvss/vsrr/drug-overdose-data.htm

overdose = read.table('Underlying Cause of Death, 1999-2019.txt', sep = "\t", nrows=21, header = T)
overdose %<>% select(Year, Deaths)

#adding the 2020 data point
overdose %<>% add_row(Year = 2020, Deaths = 92511)

colnames(overdose)[1] = 'year'
colnames(overdose)[2] = 'Overdose'

overdose$year %<>% as.character()
overdose$Country.Name = 'United States'

#joining the data
cause = left_join(cause, overdose, by = c('Country.Name', 'year'))


# Moto Vehicle Deaths ----------------------------------------------------
#https://injuryfacts.nsc.org/motor-vehicle/historical-fatality-trends/deaths-and-rates/
#2020 Data point:
#https://www.reuters.com/world/us/us-traffic-deaths-jump-105-early-2021-2021-09-02/#:~:text=For%20all%20of%202020%2C%20U.S.,a%20first%20quarter%20since%202007.

roaddeaths = read_excel('Motor-Vehicle Deaths and Rates.xlsx')

#only including the years that we have for the rest of the cause of death data
roaddeaths %<>% filter(Year >=1999)
roaddeaths$Year %<>% as.character()

roaddeaths$Country.Name = 'United States'
colnames(roaddeaths)[1] = 'year'
colnames(roaddeaths)[2] = 'Road Deaths'

cause = left_join(cause, roaddeaths, by = c('Country.Name', 'year'))





# Suicide Data Per 100,000 for Entire World -------------------------------

#reading in the suicide data for the entire world
suicide = read.csv("Suicide.csv")
suicideT = suicide[suicide$SUBJECT == "TOT",]
suicideM = suicide[suicide$SUBJECT == "MEN",]
suicideW = suicide[suicide$SUBJECT == "WOMEN",]
suicideT %<>% select(LOCATION, TIME, Value)
suicideM %<>% select(LOCATION, TIME, Value)
suicideW %<>% select(LOCATION, TIME, Value)

# renaming cols
colnames(suicideT)[1] = 'Country.Code'
colnames(suicideT)[2] = 'year'
colnames(suicideT)[3] = 'suicidePer100000T'

colnames(suicideM)[1] = 'Country.Code'
colnames(suicideM)[2] = 'year'
colnames(suicideM)[3] = 'suicidePer100000M'

colnames(suicideW)[1] = 'Country.Code'
colnames(suicideW)[2] = 'year'
colnames(suicideW)[3] = 'suicidePer100000W'


suicideT$year %<>% as.character()
suicideM$year %<>% as.character()
suicideW$year %<>% as.character()


# Infant Mortality --------------------------------------------------------

infantMortality = read.csv("InfantMortality.csv")
infantMortality %<>% select(LOCATION, TIME, Value)

colnames(infantMortality)[1] = 'Country.Code'
colnames(infantMortality)[2] = 'year'
colnames(infantMortality)[3] = 'Infant_Mortality_per_1000births'

infantMortality$year %<>% as.character()
  
# Healthcare Spending -----------------------------------------------------
#from world bank

hs<-read.csv("WorldBank-HealthSpending.csv")

colnames(hs)[1] = "Country.Name"

#pivoting the data
hs %>% pivot_longer(cols = c("X2000":"X2020"), 
                    names_to = "year", 
                    values_to = "health spending") -> hs

hs$year = str_remove(hs$year, "X")

hs %<>% select(Country.Name, Country.Code, year, `health spending`)



# World Happiness ---------------------------------------------------------

wh<-read.csv("WHR-2019.csv")

colnames(wh)[1] = "Country.Name"
colnames(wh)[2] = "year"

wh %<>% select(Country.Name, year, 'Life.Ladder')
colnames(wh)[3] = "Happiness.Index"

wh$year<-as.character(wh$year)






# Joining the Data --------------------------------------------------------

#joining all of the data
mergedData = left_join(gdp, pop, by = c('Country.Name', 'Country.Code', 'year'))
mergedData = left_join(mergedData, death, by = c('Country.Name', 'Country.Code', 'year'))
mergedData = left_join(mergedData, letotal, by = c('Country.Name', 'Country.Code', 'year'))
mergedData = left_join(mergedData, cause, by = c('Country.Name', 'year'))
mergedData = left_join(mergedData, unemployment, by = c('Country.Name', 'Country.Code', 'year'))
mergedData = left_join(mergedData, hs, by = c('Country.Name', 'Country.Code', 'year'))
mergedData = left_join(mergedData, wh, by = c('Country.Name', 'year'))
mergedData = left_join(mergedData, infantMortality, by = c('Country.Code', 'year'))
mergedData = left_join(mergedData, suicideT, by = c('Country.Code', 'year'))
mergedData = left_join(mergedData, suicideM, by = c('Country.Code', 'year'))
mergedData = left_join(mergedData, suicideW, by = c('Country.Code', 'year'))

#dropping all observations were gdp is na since we are comparing all our variables to gdp
mergedData %<>% drop_na(`gdp per capita`)

colnames(cause)

#scaling the variables to a per capita basis
mergedData %<>% mutate(cardio_per_capita = `Heart disease`/population,
                       cancer_per_capita = Cancer/population,
                       Alzheimer_per_capita = `Alzheimers disease`/population,
                       Lower_respiratory_per_capita = CLRD/population,
                       Kidney_disease_per_capita = `Kidney disease`/population,
                       Diabetes_per_capita = Diabetes/population,
                       Suicide_per_capita = Suicide/population,
                       Unintentional_injuries_per_capita = `Unintentional injuries`/population,
                       Stroke_per_capita = Stroke/population,
                       Influenza_per_capita = `Influenza and pneumonia`/population,
                       Road_injuries_per_capita = `Road Deaths`/population,
                       Overdose_per_capita = Overdose/population
                       
)

#calculating the percent change using the per capita data
mergedData %>% group_by(Country.Name) %>% summarise(year = year,
                                                    gdp_per_change = ((`gdp per capita` - lag(`gdp per capita`))/lag(`gdp per capita`))*100,
                                                    death_per_change = ((`death rate per 1000` - lag(`death rate per 1000`))/lag(`death rate per 1000`))*100,
                                                    le_per_change = ((`Life Expectancy Total` - lag(`Life Expectancy Total`))/lag(`Life Expectancy Total`))*100,
                                                    cardio_per_change = ((cardio_per_capita - lag(cardio_per_capita))/lag(cardio_per_capita))*100,
                                                    cancer_per_change = ((cancer_per_capita - lag(cancer_per_capita))/lag(cancer_per_capita))*100,
                                                    Alzheimer_per_change = ((Alzheimer_per_capita - lag(Alzheimer_per_capita))/lag(Alzheimer_per_capita))*100,
                                                    Lower_respiratory_per_change = ((Lower_respiratory_per_capita - lag(Lower_respiratory_per_capita))/lag(Lower_respiratory_per_capita))*100,
                                                    Kidney_disease_per_change = ((Kidney_disease_per_capita - lag(Kidney_disease_per_capita))/lag(Kidney_disease_per_capita))*100,
                                                    Diabetes_per_change = ((Diabetes_per_capita - lag(Diabetes_per_capita))/lag(Diabetes_per_capita))*100,
                                                    Road_injuries_per_change = ((Road_injuries_per_capita - lag(Road_injuries_per_capita))/lag(Road_injuries_per_capita))*100,
                                                    Suicide_per_change = ((Suicide_per_capita - lag(Suicide_per_capita))/lag(Suicide_per_capita))*100,
                                                    Overdose_per_change = ((Overdose_per_capita - lag(Overdose_per_capita))/lag(Overdose_per_capita))*100,
                                                    population_per_change = ((population - lag(population))/lag(population))*100,
                                                    Unintentional_injuries_per_change = ((Unintentional_injuries_per_capita - lag(Unintentional_injuries_per_capita))/lag(Unintentional_injuries_per_capita))*100,
                                                    Stroke_per_change = ((Stroke_per_capita - lag(Stroke_per_capita))/lag(Stroke_per_capita))*100,
                                                    Influenza_per_change = (( Influenza_per_capita - lag(Influenza_per_capita))/lag(Influenza_per_capita))*100
                                                    
) -> summarytable

#joining the percent change data and the merged data
mergedData = left_join(mergedData, summarytable, by = c('Country.Name', 'year'))

US=mergedData %>% select(Country.Name, year, `death rate per 1000`, death_per_change, population_per_change) %>% filter(Country.Name == "United States")

#writing to csv to upload to Tableau
write.csv(mergedData, file = "MergedData.csv", na="")




