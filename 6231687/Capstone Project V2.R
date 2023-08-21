
install.packages("rmarkdown")
install.packages("haven")
install.packages("tidyr")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("knitr")

# Google Capstone Project V2
## Understanding Migration Patterns in Bolivia
# Prepare -----------------------------------------------------------------

# The first step is preparing the data. We can begin by importing this data in
# and starting the process of preparing it for analysis. 
library("rmarkdown")
library("haven")
library("skimr")
library("janitor")
library("tidyr")
library("dplyr")
library("ggplot2")
library("stringr")
library("knitr")

# Reading in the data
df <- read_sav("Data/EH2021_Persona.sav")


# Cleaning the dataset. There's tons of info that I don't need. 
df_data = select(df,depto,area,s01a_02,s01a_03,s01a_03,s01a_09,s01a_10,s01b_11a,s01b_11b,s01b_11e,s01b_12,s04a_06,s04b_12,s04d_22a,niv_ed,cob_op)

# Now that I have the important data isolated, we can keep cleaning and verifying
# We will start with renaming columns

df_data <- df_data %>%
  rename("Department" = "depto",
         "Urban/Rural" = "area",
         "Sex" = "s01a_02",
         "Age" = "s01a_03",
         "Indiginous" = "s01a_09",
         "Civil_Status" = "s01a_10",
         "Residence_2016" = "s01b_11a",
         "Department_2016" = "s01b_11b",
         "Country_2016" = "s01b_11e",
         "Reason" = "s01b_12",
         "Info_status" = "s04a_06",
         "Employment_type" = "s04b_12",
         "Income" = "s04d_22a",
         "Education" = "niv_ed",
         "Occupation" = "cob_op")

## Now that the columns are renamed appropriately, it's time to start renaming
## some of the variables so it's easier to read and analyze.
## Example: "Sex" column is numerical (1,2), and I want it to read (Male,Female)
## as it would be easier to read at a glance. 

## Turns out, since we used a .sav (SSPS) the format of the variables is different.
## To make this analysis easier, I will convert that format to a factor format using
## the Haven library and the as_factor() method. 

df_data$Sex <- as_factor(df_data$Sex)

## A quick check confirms the changed value format
class(df_data$Sex)

## Next, we need to use the recode() method to change those labels into the 
# desired name, in this case, translating and renaming. 
df_data <- df_data %>% 
  mutate(Sex = recode(Sex, `1. Hombre` = "Male", `2. Mujer` = "Female"))

## Checking the dataframe shows exactly what we were looking for. Now to complete 
## the variable name change of the rest...
df_data$Department <- as_factor(df_data$Department)

df_data$`Urban/Rural`<- as_factor(df_data$`Urban/Rural`)

df_data$Indiginous <- as_factor(df_data$Indiginous)

df_data$Civil_Status <- as_factor(df_data$Civil_Status)

df_data$Residence_2016 <- as_factor(df_data$Residence_2016)

df_data$Department_2016 <- as_factor(df_data$Department_2016)

df_data$Country_2016 <- as_factor(df_data$Country_2016)

df_data$Reason <- as_factor(df_data$Reason)

df_data$Info_status <- as_factor(df_data$Info_status)

df_data$Education <- as_factor(df_data$Education)

df_data$Occupation <- as_factor(df_data$Occupation)


# Moving on, some of those labels are in Spanish and a bit wordy. We will 
# clean up and translate some of these variables
df_data <- df_data %>% 
  mutate(`Urban/Rural` = recode_factor(`Urban/Rural`, `Urbana` = "Urban", `Rural` = "Rural")) %>%
  mutate(Indiginous = recode_factor(Indiginous,
                                    `1. Pertenece` = "Belongs",
                                    `2. No pertenece` = "Does not belong",
                                    `3. NS/NR` = "N/A")) 
df_data <- df_data %>%
  mutate(Civil_Status = recode_factor(Civil_Status,
                                      `1. SOLTERO/A` = "Single",
                                      `2. CASADO/A` = "Married",
                                      `3. CONVIVIENTE O CONCUBINO/A` = "Commonlaw Marriage",
                                      `4. SEPARADO/A` = "Separated",
                                      `5. DIVORCIADO/A` = "Divorced",
                                      `6. VIUDO/A` = "Widowed"))
  
df_data <- df_data %>%
  mutate(Residence_2016 = recode_factor(Residence_2016,
                                        `1. Aquí` = "Here",
                                        `2. En otro lugar del país` = "Elsewhere in the country",
                                        `3. En el exterior` = "Abroad",
                                        `4. Aún no habia nacido` = "Not yet born"))

# Remove digit-dot pattern from factor levels
df_data <- df_data %>%
  mutate(Department_2016 = recode_factor(Department_2016,
                                         `01.Chuquisaca` = "Chuquisaca",
                                         `02.La Paz` = "La Paz",
                                         `03.Cochabamba` = "Cochabamba",
                                         `04.Oruro` = "Oruro",
                                         `05.Potosí` = "Potosí",
                                         `06.Tarija` = "Tarija",
                                         `07.Santa Cruz` = "Santa Cruz",
                                         `08.Beni` = "Beni",
                                         `09.Pando` = "Pando"))

# Remove numbers, periods, and spaces from the start of factor levels

df_data <- df_data %>%
  mutate(Country_2016 = str_replace_all(as.character(Country_2016), "^[0-9. ]*","") %>%
           factor())

df_data <- df_data %>%
  mutate(Reason = recode_factor(Reason,
                                `1. BÚSQUEDA DE TRABAJO` = "Job Search",
                                `2. TRASLADO DE TRABAJO` = "Work Transfer",
                                `3. EDUCACIÓN` = "Education",
                                `4. SALUD` = "Health",
                                `5. RAZÓN FAMILIAR` = "Family Reason",
                                `6. OTRA RAZÓN (Especifique)` = "Other Reason (Specify)"))

df_data <- df_data %>%
  mutate(Info_status = recode_factor(Info_status,
                                     `1. ¿Estudiante?` = "Student",
                                     `2. ¿Ama de casa o responsable de los quehaceres y/o cuidado de los miembros del hogar?` = "Stay-at-Home",
                                     `3. ¿Jubilado o benemérito?` = "Retired",
                                     `4. ¿Enfermo o con alguna discapacidad?` = "Sick/Disabled",
                                     `5. ¿Persona de edad avanzada?` = "Elderly",
                                     `6. ¿Otro? (Especifique)` = "Other"))
df_data <- df_data %>%
  mutate(Employment_type = as_factor(Employment_type)) %>%
  mutate(Employment_type = recode_factor(Employment_type,
                                         `1. Obrero/Empleado` = "Unskilled Laborer",
                                         `2. Empleador/a socio que sí recibe salario` = "Salaried Employer/Partner",
                                         `3. Trabajador/a por cuenta propia` = "Self-Employed",
                                         `4. Empleador/a o socio/a que no recibe salario` = "Non-salaried employer/partner",
                                         `5. Cooperativista de producción` = "Production coop member",
                                         `6. Trabajador/a familiar sin remuneración` = "Unpaid Laborer",
                                         `7. Aprendiz o persona en formación sin remuneración` = "Unpaid Apprentice/Trainee",
                                         `8. Empleada/o del hogar` = "Stay-at-Home")) 
df_data <- df_data %>%
  mutate(Education = recode(Education,
                            `Ninguno` = "None",
                            `Primaria incompleta` = "Incomplete Primary",
                            `Primaria completa` = "Complete Primary",
                            `Secundaria incompleta` = "Incomplete Secondary",
                            `Secundaria completa` = "Complete Secondary",
                            `Superior` = "Higher Education",
                            `Otros` = "Others"))
df_data <- df_data %>%
  mutate(Occupation = recode(Occupation,
                             `Fuerzas Armadas` = "Armed Forces",
                             `Directivos Adm. Pública y Empresas` = "Public Administration and Business Managers",
                             `Profesionales cientificos e intelectuales` = "Scientific and Intellectual Professionals",
                             `Técnicos de Nivel Medio` = "Mid-Level Technicians",
                             `Empleados de oficina` = "Office Employees",
                             `Trabajadores de Servicio y Vendedores` = "Service Workers/Merchants",
                             `Trabajadores en Agricultura, Pecuaria,Pesca y otros` = "Agriculture, Livestock, Fishing and Others",
                             `Trabajadores de la Construcción, Ind. Manufacturera y Otros` = "Construction/Manufacturing Industry Workers",
                             `Operadores de Instalaciones y Maquinarias` = "Installation/Machinery Operators",
                             `Trabajadores No Calificados` = "Unskilled Workers",
                             `Sin especificar` = "Unspecified")) 


## Time for Analysis. 

# Let's start with a general analysis of the data frame. 

# Where are the respondents coming from by department? 
ggplot(df_data, aes(x = Department)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count", title = "Bar Plot of Department Column")

# Nice. What are the top 3 departments by count?
dept_counts <- table(df_data$Department)

# Sort in descending order and select top 3
top_three_depts <- sort(dept_counts, decreasing = TRUE)[1:3]

# Print the result
print(top_three_depts)

# What is the sex distribution? 
print(table(df_data$Sex))
# Pretty evenly spread out. Slightly more female respondents.  

# What about the distribution of people living in rural
# vs urban areas? 
print(table(df_data$`Urban/Rural`))
# Much much more urban responses than rural ones. Makes sense though.

# Is there any meaningful change in the sex distribution per Department? 
# Create a stacked bar plot for sex
ggplot(df_data, aes(x = Department, fill = Sex)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count", fill = "Sex", 
       title = "Sex Distribution per Department")
# No significant difference between genders per department

# What about Urban/Rural living per department?
# Create a stacked bar plot for Urban/Rural living
ggplot(df_data, aes(x = Department, fill = `Urban/Rural`)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count", fill = "Urban/Rural", 
       title = "Urban/Rural Distribution per Department")
# Interesting! There are major differences in the distribution of rural
# and urban living per department. It makes sense though, because the departments with
# the highest amount of urban respondents also have the largest cities in the country
# (La Paz, Santa Cruz, and Cochabamba). Some departments (Potosi and Pando)
# have almost a 50-50 distribution.

# Let's look at the age of the respondents. 
ggplot(df_data, aes(x = Age)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Age", y = "Count", title = "Bar Plot of Age Column")
# Pretty much the expected bar plot. (Although, it's kind of weird that 
# there seems to be a lot of zero age though...)

# Let's look into it.

# What is the age range?
print(min(df_data$Age))
print(max(df_data$Age))
# Respondents from age 0 to 98 (My Tia Angelica was 105!!)
zero_age_count <- sum(df_data$Age == 0, na.rm = TRUE)
print(zero_age_count)
# Counting the number of zero age results in 567 entries. The zero age response
# refers to any infants in the house.

# Let's visualize the average age of each department
df_data %>% 
  group_by(Department) %>%
  summarise(AverageAge = mean(Age, na.rm = TRUE)) %>%
  ggplot(aes(x = Department, y = AverageAge)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average age per department",
       x = "Department",
       y = "Average Age")
# Awesome. Average age hovers around 30 except for in Santa Cruz, Beni and Pando
# where it is lower.
# Could that have anything to do with those zero counts? 
df_data %>% 
  group_by(Department) %>%
  summarise(zero_count = sum(Age == 0, na.rm = TRUE)) %>%
  ggplot(aes(x = Department, y = zero_count)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of zero age per department",
       x = "Department",
       y = "zero_count")
# Okay, so Santa Cruz has the most zero-age with 125. Beni and Pando have
# fewer than 100 combined so it's negligible with the average age. I think it's
# also negligible in Santa Cruz but let's check.

# Let's compare the average age before and after removing the zero age
df_data %>%
  filter(Department == "Santa Cruz") %>%
  summarise(Avg_Age = mean(Age, na.rm = TRUE))
# Average age in Santa Cruz = 28.9
df_data %>%
  filter(Department == "Santa Cruz", Age != 0) %>%
  summarise(Avg_Age_no_zero = mean(Age, na.rm = TRUE))
# Average age disregarding zero ages = 29.4
# Barely any change, but still Santa Cruz is a bit younger though not as 
# young as beni or pando. 

# Alright moving on to Education. 

# Let's see what the distribution is.
ggplot(df_data, aes(x = Education)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count", title = "Bar Plot of Education Column")
# Low amount of completed primary. We should filter out anyone below 18 years.

# Distribution of age (18+)
df_data %>%
  filter(Age >= 18) %>%
  ggplot(aes(x = Education)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Education", y = "Count", title = "Bar Plot of Education Column for people above 18")
# Interestingly, there is a larger group of people over 18 that never completed primary school. 

# Are there any differences when it comes to educational level and gender? 
df_data %>%
  filter(Age >= 18) %>%
  ggplot(aes(x = Education, fill = Sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Education", y = "Count", title = "Bar Plot of Education (18+) Comparing Gender")
# Fascinating. There is pretty much an even split across the board except
# with people who don't have any education, which is overwhelmingly female. 
# I wonder why...

# Maybe people with no education are mostly from Rural areas (generally speaking, 
# more traditionally male-dominated areas). Let's check.
df_data %>%
  filter(Education == 'None', Age >= 18) %>%
  ggplot(aes(x = `Urban/Rural`, fill = Sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Urban/Rural", y = "Count", title = "Bar Plot of Urban/Rural (18+) People with No Education Comparing Gender")
# Nope. If anything, there's more men in rural areas that haven't received education
# than in urban environments... 

# Does indiginous heritage have anything to say? 
df_data %>%
  filter(Education == 'None', Age >= 18) %>%
  ggplot(aes(x = `Indiginous`, fill = Sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Urban/Rural", y = "Count", title = "Bar Plot of Urban/Rural (18+) People with No Education Comparing Gender")
# Not really. Both identifiers are almost equal with women being the majority.
# Well, that tangent was interesting but let's move on...

# What does this Educational spread look like in every department? 
df_data %>%
  filter(Age >= 18) %>%
  ggplot(aes(x = Education)) +
  geom_bar() +
  facet_wrap(~ Department) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Education", y = "Count", title = "Bar Plot of Education Column for people above 18")
# Clearly, the three big departments have the higher number of people who completed
# Secondary and Higher Education. However, those departments also had a higher number of 
# respondents. We should normalize this data and then compare.

# One way to do this is to compare the percentage of education relative to each department.
df_data %>%
  filter(Age > 18) %>%
  count(Department, Education) %>%
  group_by(Department) %>%
  mutate(Total = sum(n),
         Percentage = n / Total * 100) %>%
  ggplot(aes(x = Education, y = Percentage, fill = Education)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Education Level", y = "Percentage",
       title = "Education level by Department (Normalized)",
       fill = "Education Level") +
  facet_wrap(~ Department)
# This is much better. Potosi, Pando, and Beni have the lowest percentage of people
# with Higher Education. Potosi and Chuquisaca both have nearly 20% of incompleted primary.

# Okay, lets move into labor and employment.

# What does the industry/occupation distribution look like? 
df_data %>%
  filter(!is.na(Occupation)) %>%
  ggplot(aes(x = Occupation)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Occupation", y = "Count", title = "Bar Plot of Occupation Column")
# There are three main industries that pretty much monopolize the labor. Let's
# dig into it. 

# What are the top three industries in percentage? 
df_data %>%
  filter(!is.na(Occupation)) %>%
  count(Occupation) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 3)
# The top three occupations/industries are the following
# 1. Service/Merchants - 4397 - 22.6 %
# 2. Agriculture Labor - 3902 - 20.1 %
# 3. Construction/Manufacturing - 3613 - 18.6 %

# What does the employment type look like? 
df_data %>%
  filter(!is.na(Employment_type)) %>%
  count(Employment_type) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  slice_max(n, n = 3)
## The top three employment types are the following
# 1. Self-Employed - 8635 - 44.5 %
# 2. Worker/Employee - 6620 - 34.1 %
# 3. Unpaid family worker - 3013 - 15.5 %

# What does the top three industries look like separated by the top three
# employment types? A break down of the worker type by industry. 

# Subset of top three occupations
top_occupations <- df_data %>%
  filter(!is.na(Occupation)) %>%
  count(Occupation) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%
  pull(Occupation)

# Subset of top three employment types
top_employment_types <- df_data %>%
  filter(!is.na(Employment_type)) %>%
  count(Employment_type) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%
  pull(Employment_type)

# Subset the data
df_subset <- df_data %>%
  filter(Occupation %in% top_occupations & Employment_type %in% top_employment_types)

# Create the stacked bar plot
ggplot(df_subset, aes(x = Occupation, fill = Employment_type)) +
  geom_bar(position = "stack") +
  labs(x = "Occupation", y = "Count", fill = "Employment Type",
       title = "Stacked Bar Plot of Top Three Occupations and Employment Types") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# This visualization is great. it shows the division of employment type in the 
# top three occupations/industries. What it shows is a large portion of all labor
# is Self-Employment. As expected, there are more Unskilled Laborers in the construction
# industry and less in the agricultural. What I didn't expect was a larger amount of 
# unpaid laborers in agriculture. However, if you consider they are most likely 
# people working their own family farm/land it begins to make sense.

# What woud this plot look like comparing each department?

# Create the stacked bar plot in every department
ggplot(df_subset, aes(x = Occupation, fill = Employment_type)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Department) +
  labs(x = "Occupation", y = "Count", fill = "Employment Type",
       title = "Stacked Bar Plot of Top Three Occupations and Employment Types") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Nothing wildly unexpected, fun visual to look at. In order for it to be useful
# we would need to change this into percentages though, to normalize the data.

# How about employment separated by sex? 

# Create the stacked bar plot separated by sex
ggplot(df_subset, aes(x = Occupation, fill = Sex)) +
  geom_bar(position = "stack") +
  labs(x = "Occupation", y = "Count", fill = "Sex",
       title = "Stacked Bar Plot of Top Three Occupations and Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the stacked bar plot in every department separated by sex
ggplot(df_subset, aes(x = Occupation, fill = Sex)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Department) +
  labs(x = "Occupation", y = "Count", fill = "Sex",
       title = "Stacked Bar Plot of Top Three Occupations and Sex") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Again, nothing out of the ordinary, but for it to be useful it should be 
# changed to show percentage not total count. 

# I"m curious about the average age in these industries.

# Filter data and calculate average age
df_data %>%
  filter(Occupation %in% top_occupations, Age >= 18) %>%
  group_by(Occupation) %>%
  summarise(Avg_Age = mean(Age, na.rm = TRUE))
# Average ages per industry are 46.5, 40.2, and 39.4.

# Now, I want to explore the pay/income. However, first I need to append a column
# I initially forgot to add.The frequency of pay.

# Bind the new column to the existing data frame
df_data <- df_data %>%
  bind_cols(df %>%
              select(s04d_22b) %>%
              rename(Pay_frequency = s04d_22b))
df_data <- df_data %>% 
  mutate(Pay_frequency = factor(Pay_frequency),
         Pay_frequency = recode_factor(Pay_frequency,
                                       `1` = "Daily",
                                       `2` = "Weekly",
                                       `3` = "Bi-weekly",
                                       `4` = "Monthly",
                                       `5` = "Bi-monthly",
                                       `6` = "Quarterly",
                                       `7` = "Bi-annually",
                                       `8` = "Annually"))

# Next, we need to normalize that frequency in a new column. We can do so by 
# calculating the monthly pay based off the frequency.
df_data <- df_data %>% 
  mutate(Monthly_Income = case_when(
    Pay_frequency == "Daily"   ~ Income * 30,  # Assuming 30 days in a month
    Pay_frequency == "Weekly"  ~ Income * 4,   # Assuming 4 weeks in a month
    Pay_frequency == "Bi-weekly" ~ Income * 2, # 2 pay periods in a month
    Pay_frequency == "Monthly" ~ Income,       # Income is already monthly
    Pay_frequency == "Bi-monthly" ~ Income / 2, # 2 months per pay period
    Pay_frequency == "Quarterly" ~ Income / 3,  # 3 months per pay period
    Pay_frequency == "Bi-annually" ~ Income / 6, # 6 months per pay period
    Pay_frequency == "Annually" ~ Income / 12,  # 12 months per pay period
    TRUE ~ NA_real_  # In case there are unexpected values
  ))

# Great! Now let's analyze.

# What is the range and mean monthly income? 

  df_data %>%
    filter(!is.na(Monthly_Income)) %>%
    summarise(
      Maximum = max(Monthly_Income),
      Minimum = min(Monthly_Income),
      Median = median(Monthly_Income),
      Mean = mean(Monthly_Income)
    )
  

# The lowest paid person gets 16.667 bs per month...
# The highest gets 452,400 bs per month...
# The average monthly income is 5,089.5 bs. 
# And the minimum wage as per Bolivian law is 2,250 bs. 
# Either something is wrong or that is one lucky person. Let's dig in.

# Let's look at the outliers in a separate table.
outliers <- df_data %>%
  filter(!is.na(Monthly_Income), Monthly_Income > 70000)
# I defined 70000 bs a month as a limit because it is liberally the equivalent
# of $120,000 per year. Maybe software developers, upper level management, and 
# some retailers either remote or not could bring that kind of dough in. There are 
# only 17 people in that range.

# Does a histogram or box plot tell us anything? 

# Monthly Income Histogram
df_data %>%
  filter(!is.na(Monthly_Income),Monthly_Income < 70000) %>%
  ggplot(aes(x = Monthly_Income)) +
  geom_histogram(bins = 150, fill = 'steelblue', color = 'black') +
  theme_minimal() +
  labs(x = "Monthly Income", y = "Frequency",
       title = "Distribution of Monthly Income")
# Monthly Income Box Plot
df_data %>%
  filter(!is.na(Monthly_Income), Monthly_Income < 70000) %>%
  ggplot(aes(x = "", y = Monthly_Income)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "", y = "Monthly Income",
       title = "Box Plot of Monthly Income")
# Just visualizes those inequities. 

# Based visually on the histogram, setting a limit at 20,000 Bs. per month 
# (roughly, $3,000 dollars monthly) 
# Monthly Income Histogram
df_data %>%
  filter(!is.na(Monthly_Income),Monthly_Income < 20000) %>%
  ggplot(aes(x = Monthly_Income)) +
  geom_histogram(bins = 150, fill = 'steelblue', color = 'black') +
  theme_minimal() +
  labs(x = "Monthly Income", y = "Frequency",
       title = "Distribution of Monthly Income")

# Out of curiosity, what is the average monthly income per department? 
df_data %>%
  filter(!is.na(Monthly_Income)) %>%
  group_by(Department) %>%
  summarise(avg_income = mean(Monthly_Income)) %>%
  ggplot(aes(x = Department, y = avg_income)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Average Monthly Income", title = "Average Monthly Income per Department")
# Well, Santa Cruz has the highest average followed by Beni and Pando, which is a 
# little unexptected. How would that change removing some of the "outliers"?
df_data %>%
  filter(!is.na(Monthly_Income),Monthly_Income > 50000) %>%
  group_by(Department) %>%
  summarise(avg_income = mean(Monthly_Income)) %>%
  ggplot(aes(x = Department, y = avg_income)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Average Monthly Income", title = "Average Monthly Income per Department")
# Seems like the high earners really like living in Santa Cruz and Beni.

### Meat and Potato Analysis

# How many respondents are answering from the same Department where they
# were located in 5 years earlier from date of survey (2016)? 
df_data %>%
  filter(Residence_2016 != 'Here', Residence_2016 != 'Not yet born', !is.na(Residence_2016)) %>%
  count()
# Only 1,564 respondents have moved or migrated. 

# Make a separate data frame that includes only the migrants. 
df_migrants <- df_data %>%
  filter(Residence_2016 != 'Here', Residence_2016 != 'Not yet born', !is.na(Residence_2016))
  
# Lets visualize the migrant patterns
df_migrants %>%
  ggplot(aes(x = Residence_2016)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Residence in 2016", y = "Count",
       title = "Distribution of Residence 5 Years Ago (2016)")
# As expected, there is a lot more movement at the national level. 

# I am curious about the smaller abroad group. 

#Where are Bolivians moving from abroad?
df_migrants %>%
  filter(Residence_2016 == "Abroad") %>%
  ggplot(aes(x = Country_2016)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Country in 2016", y = "Count",
       title = "What country were they in 2016?")
# Lot's of people coming in from Argentina.The top 7 countries are all Hispanic
# and all but Spain are in South America. Only a handful of people coming from 
# the US and Europe. 

# Is there anything significant with Sex and migration from abroad?
df_migrants %>%
  filter(Residence_2016 == "Abroad") %>%
  ggplot(aes(x = Country_2016, fill = Sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Country in 2016", y = "Count",
       title = "What country were they in 2016?")
# Pretty even other than in a few countries dominated by male migration. Deportation?

# What is the average age? 
df_migrants %>%
  filter(Residence_2016 == "Abroad") %>%
  group_by(Country_2016) %>%
  summarise(Avg_Age = mean(Age, na.rm = TRUE)) %>%
  ggplot(aes(x = Country_2016, y = Avg_Age)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Country in 2016", y = "Average Age",
       title = "Average age of migrants by country of residence in 2016")
# There seems to be a trend with higher average age coming from higher income
# countries (US, France, Spain, Italy). Could they be coming back to retire?

# What is the top reason for people to move by country? 
df_migrants %>%
  filter(Residence_2016 == "Abroad") %>%
  group_by(Country_2016, Reason) %>%
  summarise(Count = n()) %>%
  slice(which.max(Count)) %>%
  ungroup()
# Well that doesn't say too much. Top reason for leaving is "Family Reason"

# Top employment type?
df_migrants %>%
  filter(Residence_2016 == "Abroad") %>%
  group_by(Country_2016, Employment_type) %>%
  summarise(Count = n()) %>%
  slice(which.max(Count)) %>%
  ungroup()
# Mostly unskilled laborers... 

# What department are they living in now?
df_migrants %>%
  filter(Residence_2016 == "Abroad") %>%
  group_by(Department, Country_2016) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Department, -Count) %>%
  group_by(Department) %>%
  slice_max(order_by = Count, n = 3) %>%
  ggplot(aes(x = Department, y = Count, fill = Country_2016)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count",
       title = "Top Three Countries of Origin per Department",
       fill = "Country")

# Let's turn our attention back to the national migratory patterns. 

# Where are the respondents currently living? 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country") %>%
  ggplot(aes(x = Department)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count",
       title = "Current Department of Residence (2021)",
       fill = "Department")
# Well, I expected Santa Cruz to have the highest count. Instead, it's in third
# behind La Paz and Cochabamba with over 250 people moving there. This is effectively 
# showing where people are moving too. 

# Where are they moving from?
# First, it's possible for people to have moved within their own department, which
# would show up as a move. They need to be filtered out, as this analyis is only looking
# at inter-departmental migration. 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country", as.character(Department) != as.character(Department_2016)) %>%
  ggplot(aes(x = Department_2016)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Count",
       title = "Department of Origin (2016)",
       fill = "Department")
# Intersting that the top three departments of origin since 2016 are La Paz, 
# Santa Cruz, and Cochabamaba in that order. They are hovering around 250 responses. 
# This is effectively telling us where people are moving away from. 

# So what does the net migratory change look like per department? 
# Calculate counts for each department in the current year and in 2016
current_counts <- df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country",as.character(Department) != as.character(Department_2016)) %>%
  group_by(Department) %>%
  summarise(Count = n(), .groups = "drop")

previous_counts <- df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country",as.character(Department) != as.character(Department_2016)) %>%
  group_by(Department_2016) %>%
  summarise(Previous_Count = n(), .groups = "drop")

# Merge these counts into one data frame
df_counts <- left_join(current_counts, previous_counts, by = c("Department" = "Department_2016"))

# Calculate net change for each department
df_counts <- df_counts %>%
  mutate(Net_Change = Count - Previous_Count)

# Plot bar chart of net change
ggplot(df_counts, aes(x = Department, y = Net_Change, fill = Net_Change < 0)) +
  geom_col() +
  scale_fill_manual(values = c("blue", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department", y = "Net Change", 
       title = "Net Change in Population by Department",
       fill = "Decrease") 
# Based off this data, Santa Cruz has the highest migratory difference, in the 
# negative direction. More people are leaving Santa Cruz than they are coming in 
# by quite a large margin. La Paz, Oruro and Pando all have roughly the same net
# change, though La Paz is the only one that's negative. 

# Let's dig into the migrators from Santa Cruz

# Where did the people that currently live in Santa Cruz come from? 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country", Department == "Santa Cruz",
         as.character(Department) != as.character(Department_2016)) %>%
  ggplot(aes(x = Department_2016)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department of Origin", y = "Count",
       title = "Departments of Origin of Migrants Currently Living in Santa Cruz",
       fill = "Department_2016")
# So, most migrants into Santa Cruz are coming from Cochabamba, followed by La Paz and 
# Chuquisaca. 

# What are the main reasons for this move? 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country", Department == "Santa Cruz",
         as.character(Department) != as.character(Department_2016)) %>%
  ggplot(aes(x = Reason)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Reason", y = "Count",
       title = "Reasons for people moving to Santa Cruz",
       fill = "Department_2016")
# The most common reason has to do with Family. Although, the second most popular reason
# by a large margin is job search. This falls inline with the economic growth of 
# Santa Cruz. 

# Are there any differences in the Sex of migrants into Satna Cruz? 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country", Department == "Santa Cruz",
         as.character(Department) != as.character(Department_2016)) %>%
  ggplot(aes(x = Department_2016, fill = Sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Department of Origin", y = "Count",
       title = "Departments of Origin of Migrants Currently Living in Santa Cruz Separated by Sex",
       fill = "Sex")

# Where are the people leaving moving too? 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country", Department_2016 == "Santa Cruz",
         as.character(Department) != as.character(Department_2016)) %>%
  ggplot(aes(x = Department, fill = Sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Current Department", y = "Count",
       title = "Current Departments of Emmigrants of Santa Cruz Separated by Sex",
       fill = "Sex")
# I find it interesting that people from Santa Cruz have emmigrated to Cochabamba,
# Beni, and Chuquisaca respectively. 

# Are they now living in urban areas? 
df_migrants %>%
  filter(Residence_2016 == "Elsewhere in the country", Department == "Santa Cruz",
         as.character(Department) != as.character(Department_2016)) %>%
  ggplot(aes(x = `Urban/Rural`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Urban or Rural", y = "Count",
       title = "Count of People Living in Urban or Rural",
       fill = "Sex")







