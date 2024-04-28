file_path <- file.choose()
df_Accident_Information <- read.csv(file_path)

file_path <- file.choose()
df_Vehicle_Information <- read.csv(file_path)

library(dplyr)

# вибираємо 2 транспортні засоби
df_Accident_Information_2 <- filter(df_Accident_Information, Number_of_Vehicles == 2 )

# вибираємо роки 2011-2015
df_Accident_Information_3 <- filter(df_Accident_Information_2, Year %in% c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015))

# вибираємо колонки
c_Accident <- select(df_Accident_Information_3,
                     Accident_Index, Accident_Severity, Carriageway_Hazards, Date, Day_of_Week,
                     Junction_Detail,
                     Light_Conditions,  Number_of_Casualties,
                     Road_Surface_Conditions, Road_Type, Speed_limit,
                     Urban_or_Rural_Area, Weather_Conditions, Year, Time
)

# вибираємо колонки
c_Vehicle <- select(df_Vehicle_Information,
                    Accident_Index,
                    Age_Band_of_Driver,
                    Age_of_Vehicle,
                    Sex_of_Driver,
                    make,
                    Vehicle_Manoeuvre,
                    Vehicle_Type,
                    X1st_Point_of_Impact
)


# з c_Vehicle видаляємо індекси яких немає в c_Accident
c_Vehicle_Index <- c_Vehicle %>%
  filter(Accident_Index %in% c_Accident$Accident_Index)

# з c_Accident видаляємо індекси яких немає в c_Vehicle_Index
c_Accident_Index <- c_Accident %>%
  filter(Accident_Index %in% c_Vehicle_Index$Accident_Index)

# вибір 2 водіїв з 2 датасету
duplicated_accidents <- c_Vehicle_Index %>%
  group_by(Accident_Index) %>%
  filter(n() == 2)

c_Vehicle_Index_filtered <- c_Vehicle_Index  %>%
  filter(Accident_Index %in% duplicated_accidents$Accident_Index)

# знову з c_Accident_Index видаляємо індекси яких немає в c_Vehicle_Index_filtered
c_Accident_Index_filtered <- c_Accident_Index %>%
  filter(Accident_Index %in% c_Vehicle_Index_filtered$Accident_Index)


df1 <- c_Vehicle_Index_filtered %>%
  filter(row_number() %% 2 == 1)  # Відбирає непарні рядки

df2 <- c_Vehicle_Index_filtered %>%
  filter(row_number() %% 2 == 0)  # Відбирає парні рядки


s_df1 <- df1 %>%
  rename(
    Age_Band_of_Driver1=Age_Band_of_Driver,
    Age_of_Vehicle1=Age_of_Vehicle,
    
    Sex_of_Driver1=Sex_of_Driver,
    Car_Brand1=make,
    Vehicle_Manoeuvre1=Vehicle_Manoeuvre,
    Vehicle_Type1=Vehicle_Type,
    Point_of_Impact1=X1st_Point_of_Impact
  )

s_df2 <- df2 %>%
  rename(
    Age_Band_of_Driver2=Age_Band_of_Driver,
    Age_of_Vehicle2=Age_of_Vehicle,
    Sex_of_Driver2=Sex_of_Driver,
    Car_Brand2=make,
    Vehicle_Manoeuvre2=Vehicle_Manoeuvre,
    Vehicle_Type2=Vehicle_Type,
    Point_of_Impact2=X1st_Point_of_Impact
  )


# Об'єднати за колонкою Accident_Index
merged_Vehicle <- merge(s_df1, s_df2, by = "Accident_Index", all = TRUE)

# Об'єднати за колонкою Accident_Index
all_data1 <- merge(c_Accident_Index_filtered , merged_Vehicle, by = "Accident_Index", all = TRUE)

# видалення na
all_data2 <- na.omit(all_data1)

selected_columns <- c(
  "Accident_Index"  ,
  "Accident_Severity","Carriageway_Hazards"
  ,"Date"
  ,"Day_of_Week"
  ,"Junction_Detail"
  ,"Light_Conditions"
  , "Number_of_Casualties"
  ,"Road_Surface_Conditions"
  , "Road_Type"
  , "Speed_limit"
  , "Urban_or_Rural_Area"
  , "Weather_Conditions"
  , "Year"
  , "Time"
  , "Age_Band_of_Driver1"
  , "Age_of_Vehicle1"
  , "Sex_of_Driver1"
  , "Car_Brand1"
  ,"Vehicle_Manoeuvre1"
  , "Vehicle_Type1"
  , "Point_of_Impact1"
  , "Age_Band_of_Driver2"
  , "Age_of_Vehicle2"
  , "Sex_of_Driver2"
  , "Car_Brand2"
  , "Vehicle_Manoeuvre2"
  , "Vehicle_Type2"
  , "Point_of_Impact2"
)

all_data3 <- all_data2[, selected_columns]

# Видаляємо рядки, де хоча б одна колонка містить "Data missing or out of range"
filtered_data <- all_data3 %>%
  filter_all(all_vars(. != "Data missing or out of range"))


# write.csv(filtered_data, file = "C:/Users/User/Desktop/filtered_data.csv", row.names = FALSE)

filtered_data_1 <- filtered_data[, selected_columns]

# Видаляємо рядки, де хоча б одна колонка містить "Unknown", "Not known",
filtered_data_2 <- filtered_data_1 %>%
  filter_all(all_vars(!(. %in% c("Unknown", "Not known", "Unallocated"))))


filtered_data_3 <- filtered_data_2  %>%
  rename(
    Area_Type=Urban_or_Rural_Area,
    
  )


library(lubridate)

# Перетворення колонки "Date" на тип дати
filtered_data_3$Date <- as.Date(filtered_data_3$Date)

# Додавання нових колонок для дня та місяця
filtered_data_3$Day <- day(filtered_data_3$Date)
filtered_data_3$Month <- month(filtered_data_3$Date)

# Видалення оригінальної колонки "Date", якщо потрібно
filtered_data_3 <- subset(filtered_data_3, select = -Date)

################################################################################################################################
# Графік 0 (Слайд 7). Корелограма всіх числових змінних датасету
install.packages("GGally")
library(GGally)
# Коефіцієнт кореляції Пірсона
ggcorr(filtered_data_3 %>% select (7,10,13,16,23,29,30), label=TRUE)
# Коефіцієнт кореляції Спірмана
ggcorr(filtered_data_3 %>% select (7,10,13,16,23,29,30), label=TRUE,method = c("pairwise", "spearman"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 1.1. (Слайд 10). Кількість аварій і їх серйозність у міській і сільській місцевості у відсотковому відношенні
library(ggplot2)
accident_area_percentage <- filtered_data_3 %>%
  filter(Area_Type != "Unallocated") %>%
  group_by(Area_Type) %>%
  summarise(percentage = n() / nrow(filtered_data_3) * 100)

accident_area_histogram <- ggplot(accident_area_percentage, aes(x = Area_Type, y = percentage, fill = Area_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "black", show.legend = FALSE) +
  labs(title = "Відсоток аварій на місцевість",
       x = "Тип місцевості", y = "Відсоток аварій", fill = "Серйозність аварії") +
  scale_fill_manual(values = c("lightblue", "green"), labels = c("Сільська", "Міська")) +
  scale_x_discrete(labels = c("Сільська", "Міська"))
theme_minimal()

print(accident_area_histogram)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 1.2. (Слайд 10). Кількість аварій і їх серйозність у міській і сільській місцевості

severity_by_area <- filtered_data_3%>%
  group_by(Area_Type, Accident_Severity) %>%
  summarize(Count = n())

ggplot(severity_by_area, aes(x = Area_Type, y = Count, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Міська або сільська місцевість", y = "Кількість аварій", fill = "Серйозність аварії") +
  scale_fill_manual(values = c("lightcoral", "green", "skyblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = c("Сільська", "Міська")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 10))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 1.3. (Слайд 11). Відсоток аварій за ступенем серйозності та типом місцевості

accident_area_counts <- filtered_data_3 %>%
  group_by(Accident_Severity, Area_Type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
accident_area_chart <- ggplot(accident_area_counts, aes(x = Area_Type, y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            size = 3, color = "black", vjust = -0.5) +
  labs(title = "        Відсоток дорожньо-транспортних пригод \n     за ступенем серйозності та типом місцевості",
       x = "Тип місцевості", y = "Відсоток аварій", fill = "Серйозність аварії") +
  
  scale_fill_manual(values = c("red", "orange", "yellow"), labels = c("Фатальна", "Серйозна", "Легка")) +  # Встановимо кольори
  scale_x_discrete(labels = c("Сільська", "Міська")) +
  theme_minimal()

print(accident_area_chart)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 1.4. (Слайд 12). Залежність типу дороги від серйозності аварії

ggplot(filtered_data_3, aes(x = Road_Type, y = Accident_Severity)) +
  geom_count() +
  labs(x = "Тип дороги", y = "Серйозність аварії") +
  scale_x_discrete(labels = c("Двопроїзна", "Одностороння\nвулиця", "Кругова", "Однопроїзна", "Підйомна\nдорога",  "Невідомо")) +
  scale_y_discrete(labels = c("Критичний", "Серйозний", "Незначний")) +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 2.1. (Слайд 13). Топ-10 найпопулярніших марок автомобілів із найбільшою кількістю аварій

accidents_by_brand_severity <- filtered_data_3 %>%
  group_by(Car_Brand1, Accident_Severity) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_10_brands <- accidents_by_brand_severity %>%
  filter(Car_Brand1 %in% head(unique(accidents_by_brand_severity$Car_Brand1), 10))

ggplot(top_10_brands, aes(x = reorder(Car_Brand1, count), y = count, fill = Accident_Severity)) +
  geom_bar(stat = "identity") +
  labs(x = "Марка автомобіля", y = "Кількість аварій", fill = "Тяжкість аварії", title = "        Топ-10 найпопулярніших марок автомобілів \n           із найбільшою кількістю аварій") +
  scale_fill_manual(values = c("red", "orange", "lightblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 2.2. (Слайд 13). Топ-10 найпопулярніших марок автомобілів

combined_car_brands <- c(filtered_data_3$Car_Brand1, filtered_data_3$Car_Brand2)
car_brands_df <- data.frame(Car_Brand = combined_car_brands)

top_ten_brands <- car_brands_df %>%
  group_by(Car_Brand) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  top_n(10, wt = count) %>%
  arrange(percentage)

ggplot(top_ten_brands, aes(x = reorder(Car_Brand, percentage), y = percentage, fill = Car_Brand)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3, color = "black") +  # Add percentage labels
  labs(title = "    Топ-10 найпопулярніших марок автомобілів",
       x = "Марка автомобіля", y = "Відсоток випадків", fill = "Марка автомобіля") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 2.3. (Слайд 14). Відсотковий розподіл віку транспортного засобу в аваріях

combined_age <- c(filtered_data_3$Age_of_Vehicle1, filtered_data_3$Age_of_Vehicle2)

age_df <- data.frame(Age_of_Vehicle = combined_age)
age_df <- age_df %>%
  mutate(Age_Category = case_when(
    Age_of_Vehicle >= 1 & Age_of_Vehicle <= 17 ~ as.character(Age_of_Vehicle),
    Age_of_Vehicle > 17 ~ "17+"
  ))

age_counts <- age_df %>%
  group_by(Age_Category) %>%
  summarise(count = n())

age_counts <- age_counts %>%
  mutate(percentage = (count / sum(count)) * 100)

age_counts$Age_Category <- factor(age_counts$Age_Category,
                                  levels = c(as.character(1:17), "17+"))

age_bar_chart <- ggplot(age_counts, aes(x = Age_Category, y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3, color = "black") +  # Add percentage labels
  labs(title = "       Відсотковий розподіл віку транспортного засобу в аваріях",
       x = "Вік транспортного засобу", y = "Відсоток аварій") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

print(age_bar_chart)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 2.4. (Слайд 15). Топ-10 автомобільних марок за кількістю смертельних аварій

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

fatal_accidents_by_car <- fatal_accidents %>%
  group_by(Car_Brand1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_10_cars_fatal <- fatal_accidents_by_car %>%
  slice(1:10)

ggplot(top_10_cars_fatal, aes(x = reorder(Car_Brand1, count), y = count)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Марка автомобіля", y = "Кількість смертельних аварій", title = "                 Топ-10 автомобільних марок \n              за кількістю смертельних аварій") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 2.5. (Слайд 15). Топ-10 вікових категорій водіїв за кількістю смертельних аварій

fatal_accidents <- filtered_data_3 %>%
  filter(Accident_Severity == "Fatal")

fatal_accidents_by_car <- fatal_accidents %>%
  group_by(Age_of_Vehicle1) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_10_cars_fatal <- fatal_accidents_by_car %>%
  slice(1:10)

ggplot(top_10_cars_fatal, aes(x = reorder(Age_of_Vehicle1, count), y = count)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Вікова категорія", y = "Кількість смертельних аварій", title = "                   Топ-10 вікових категорій водіїв \n                 за кількістю смертельних аварій") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 3.1. (Слайд 16). Частота нещасних випадків (аварій) за часом та днем

accident_data_timed <- filtered_data_3 %>%
  mutate(Time = as.POSIXct(Time, format = "%H:%M"),
         Day_of_Week = factor(Day_of_Week, levels = c("Sunday","Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")),
         Hour = sprintf("%02d:00", hour(ceiling_date(Time, "hour")))) %>%
  select(Day_of_Week, Hour)

accident_counts_timed <- accident_data_timed %>%
  group_by(Day_of_Week, Hour) %>%
  summarise(count = n())


heatmap <- ggplot(accident_counts_timed, aes(x = Hour, y = Day_of_Week, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(x = "Час доби", y = "День тижня", title = "Частота нещасних випадків за часом та днем",fill = "Кількість") +
  scale_y_discrete(labels = c("Неділя","Субота", "П'ятниця", "Четвер", "Середа", "Вівторок","Понеділок")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = sprintf("%02d:00", 0:23))

print(heatmap)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 3.2. (Слайд 16). Середня кількість нещасних випадків за місяцями та ступенями тяжкості


accidents_by_month <- filtered_data_3 %>%
  group_by(Month, Accident_Severity) %>%
  summarise(mean_accidents = mean(n())) %>%
  ungroup()


accidents_by_month$Month <- reorder(accidents_by_month$Month, accidents_by_month$mean_accidents, FUN = mean)


bar_plot <- ggplot(accidents_by_month, aes(x = Month, y = mean_accidents, fill = Accident_Severity)) +
  geom_bar(stat = "identity") +
  labs(x = "Місяць", y = "Середній показник нещасних випадків", title = "   Середня кількість нещасних випадків \n     за місяцями та ступенем тяжкості", fill = "Серйозність аварії") +
  scale_fill_manual(values = c("red", "orange", "lightblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  theme_minimal()


print(bar_plot)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 3.3. (Слайд 17). Середня кількість аварій в різні дні

holiday_dates <- c("24-12", "25-12", "31-12", "01-01", "31-10", "17-03")
holiday_accidents <- filtered_data_3 %>%
  filter((Month == 12 & Day %in% c(24, 25, 31)) |
           (Month == 1 & Day == 1) |
           (Month == 10 & Day == 31) |
           (Month == 3 & Day == 17))
weekday_mean_accidents <- filtered_data_3 %>%
  filter(Day_of_Week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  summarise(mean_weekday_accidents = mean(n() / n_distinct(Day)))

weekend_mean_accidents <- filtered_data_3 %>%
  filter(Day_of_Week %in% c("Saturday", "Sunday")) %>%
  summarise(mean_weekend_accidents = mean(n() / n_distinct(Day)))

holiday_mean_accidents <- holiday_accidents %>%
  group_by(Month, Day) %>%
  summarise(mean_holiday_accidents = mean(n()))

bar_data <- bind_rows(
  data.frame(Type = "Будній день", Mean_Accidents = weekday_mean_accidents$mean_weekday_accidents),
  data.frame(Type = "Вихідні", Mean_Accidents = weekend_mean_accidents$mean_weekend_accidents),
  holiday_mean_accidents %>% mutate(Type = paste(Month, Day, sep = "-")) %>% rename(Mean_Accidents = mean_holiday_accidents)
)


bar_chart <- ggplot(bar_data, aes(x = Type, y = Mean_Accidents, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Mean_Accidents, 1)), vjust = -0.5, size = 3, color = "black") +
  theme_minimal() +
  labs(title = "                    Середня кількість аварій", x = NULL, y = "Середня кількість аварій") +
  scale_fill_manual(values = c(rep("lightblue", 6), rep("orange", nrow(holiday_mean_accidents))))

print(bar_chart)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 4.1. (Слайд 17). Вплив вікової категорії водія на серйозність аварії

p <- ggplot(filtered_data_3, aes(x = Age_Band_of_Driver2)) +
  geom_bar(aes(fill = "Age Band of Driver"), position = "dodge") +
  labs(x = "Вікова категорія водія", y = "Кількість")


p + geom_bar(data = filtered_data_3, aes(x = Age_Band_of_Driver2, fill = Accident_Severity), position = "dodge") +
  labs(fill = "Тяжкість аварії") +
  scale_fill_manual(values = c("LightSteelBlue", "red", "orange","ForestGreen"), labels = c("Вікова категорія водія","Фатальна", "Серйозна", "Легка")) +
  theme(legend.position = "top")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 4.2. (Слайд 18). Вплив статі водія на тяжкість і кількість ДТП

ggplot(filtered_data_3, aes(x = factor(Sex_of_Driver1), fill = Accident_Severity)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = paste( ..count..)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Стать водія", y = "Кількість аварій", fill = "Серйозність аварії",title ="  Вплив статі водія на тяжкість і кількість ДТП") +
  scale_fill_manual(values = c("red", "orange", "lightblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = c("Жіноча", "Чоловіча")) +
  theme(axis.text.x = element_text(hjust = 1))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 4.3. (Слайд 19). Розподіл маневрів автомобіля за статтю водія

accidents_by_manoeuvre_sex <- filtered_data_3 %>%
  group_by(Vehicle_Manoeuvre1, Sex_of_Driver1) %>%
  summarise(count = n()) %>%
  arrange(Vehicle_Manoeuvre1)

ggplot(accidents_by_manoeuvre_sex, aes(x = Vehicle_Manoeuvre1, y = count, fill = Sex_of_Driver1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Маневр автомобіля", y = "Кількість аварій", fill = "Стать водія", title = "Розподіл маневрів автомобіля за статтю водія") +
  scale_fill_manual(values = c("IndianRed","lightblue"), labels = c("Жіноча","Чоловіча")) +
  scale_x_discrete(labels = c("Зміна смуги наліво", "Зміна смуги направо", "Попереду лівий поворот", "Попереду інше", "Попереду правий поворот", "Зрушив з місця", "Ближній обгін","Обгін автомобіля, \nщо рухається- праворуч","Обгін автомобіля, \nщо стоїть- праворуч","Парковка","Реверс","Уповільнення або зупинка","Поворот ліворуч","Поворот праворуч","Розворот","Очікування на рух - затримка","Очікування на лівий поворот","Очікування на правий поворот")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 4.4. (Слайд 20). Heatmap ДТП за віковою категорією водія та віком транспортного засобу

filtered_data <- filtered_data_3[filtered_data_3$Age_of_Vehicle1 <= 20,]
ggplot(filtered_data, aes(x = Age_Band_of_Driver1, y = Age_of_Vehicle1)) +
  stat_bin_2d(bins = 18, aes(fill = ..count..)) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  
  labs(title = "     Heatmap ДТП за віковою категорією \n      водія та віком транспортного засобу", x = "Вік водія", y = "Вік машини", fill = "Кількість аварій") +
  theme_minimal()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.1. (Слайд 21). Lolypop Погодні умови під час ДТП

library(scales)
weather_conditions_count <- filtered_data_3 %>%
  count(Weather_Conditions)

ggplot(data = weather_conditions_count, aes(x = Weather_Conditions, y = n)) +
  geom_point(color = "blue", size = 2) +
  geom_segment(aes(x = Weather_Conditions, xend = Weather_Conditions, y = 0, yend = n), color = "black") +
  labs(x = "Метеорологічні умови", y = "Кількість аварій") +
  scale_y_continuous(labels = comma_format())+
  scale_x_discrete(labels = c("Ясно+\nсильний\nвітер" ,"Без\nсильного\nвітру","Туман\nабо\nмряка","Інша","Дощ +\nсильний\nвітер", "Дощ без\nсильного\nвітру" ,"Сніг+\nсильний\nвітер" , "Сніг\nбез\nсильного\nвітру"  ))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.2. (Слайд 21). Розподіл ДТП за ступенем тяжкості та погодними умовами

top_weather_conditions <- filtered_data_3 %>%
  group_by(Weather_Conditions) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  pull(Weather_Conditions)

weather_accident_counts_filtered <- filtered_data_3 %>%
  filter(Weather_Conditions %in% top_weather_conditions) %>%
  group_by(Weather_Conditions, Accident_Severity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)


bar_plot <- ggplot(weather_accident_counts_filtered, aes(x = Weather_Conditions, y = percentage, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Погодні умови", y = "Частка нещасних випадків", title = "Розподіл ДТП за ступенем тяжкості та погодними умовами", fill = "Серйозність аварії") +
  scale_x_discrete(labels = c("Ясно+\nсильний\nвітер" ,"Без\nсильного\nвітру","Туман\nабо\nмряка","Інша","Дощ +\nсильний\nвітер", "Дощ без\nсильного\nвітру" ,"Сніг+\nсильний\nвітер" , "Сніг\nбез\nсильного\nвітру"  ))+
  scale_fill_manual(values = c("red", "orange", "lightblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_plot)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.3. (Слайд 22). Залежність кількості аварій від світлових умов

ggplot(filtered_data_3, aes(x = Light_Conditions)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7, position = "dodge") +
  labs(x = "Світлові умови", y = "Кількість") +
  scale_x_discrete(labels = c("Темрява - \n освітлення \n невідоме", "Темрява - \n вогні \n горять", "Темрява - \n вогні \n не горять", "Темрява - \n освітлення \n немає", "Денне світло")) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.4. (Слайд 22). Залежність відсотка кількості аварій від світлових умов

severity_by_light <- filtered_data_3 %>%
  group_by(Light_Conditions, Accident_Severity) %>%
  summarize(Count = n())

total_counts <- severity_by_light %>%
  group_by(Light_Conditions) %>%
  summarize(Total_Count = sum(Count))

severity_by_light <- merge(severity_by_light, total_counts, by = "Light_Conditions")

severity_by_light <- severity_by_light %>%
  mutate(Percent = Count / Total_Count * 100)

ggplot(severity_by_light, aes(x = Light_Conditions, y = Percent, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "fill") +
  # geom_text(aes(label = paste0(round(Percent), "%")), position = position_fill(vjust = 0.5), color = "white", size = 3) +
  labs(x = "Світлові умови", y = "Відсоток кількості аварій", fill = "Серйозність аварії") +
  scale_x_discrete(labels = c("Темрява-\nджерело світла\nневідоме","Темрява-\nсвітло\nввімкнено","Темрява-\nсвітло\nне ввімкнено","Темрява-\nосвітлення\nвідсутнє","Денне\nсвітло"))+
  scale_fill_manual(values = c("lightcoral", "green", "skyblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12)
  )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.5. (Слайд 23). Кількість аварій за станом дорожнього покриття та серйозністю

severity_by_road <- filtered_data_3 %>%
  group_by(Road_Surface_Conditions, Accident_Severity) %>%
  summarize(Count = n())


severity_by_road <- severity_by_road %>%
  group_by(Road_Surface_Conditions) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup()


ggplot(severity_by_road, aes(x = Road_Surface_Conditions, y = Count, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste(round(Percent, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +  # Додавання текстових міток з округленням до однієї цифри
  labs(x = "Стан дорожнього покриття",
       y = "Кількість аварій",
       title = "Кількість аварій за станом дорожнього покриття та серйозністю", fill = "Серйозність аварії") +
  scale_fill_manual(values = c("lightcoral", "green", "skyblue"),
                    labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = c("Сухо","Затоплення\nпонад 3 см","Мороз\nабо\nлід","Сніг","Вологе"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))+
  scale_y_continuous(labels = scales::comma)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.5.1. (Слайд 23). Кількість аварій за станом дорожнього покриття та серйозністю (Логарифмізований графік)

severity_by_road <- filtered_data_3 %>%
  group_by(Road_Surface_Conditions, Accident_Severity) %>%
  summarize(Count = n())

severity_by_road <- severity_by_road %>%
  group_by(Road_Surface_Conditions) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup()

ggplot(severity_by_road, aes(x = Road_Surface_Conditions, y = Count, fill = Accident_Severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste(round(Percent, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  labs(x = "Стан дорожнього покриття",
       y = "Кількість аварій",
       title = "Кількість аварій за станом дорожнього покриття та серйозністю", fill = "Серйозність аварії") +
  scale_fill_manual(values = c("lightcoral", "green", "skyblue"),
                    labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = c("Сухо","Затоплення\nпонад 3 см","Мороз\nабо\nлід","Сніг","Вологе"))+
  scale_y_continuous(labels = scales::comma, breaks = c(10, 100,500, 1000, 5000, 10000, 25000, 50000, 100000), trans = "log10") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))





# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.6. (Слайд 24). Залежність кількості і тяжкості аварії від небезпеки проїжджої частини

filtered_data_without_none <- filtered_data_3[filtered_data_3$Carriageway_Hazards != "None", ]

# Загальна кількість кожного виду Carriageway_Hazard
total_counts <- length(filtered_data_without_none$Carriageway_Hazards)

# Розрахунок кількості для кожного Carriageway_Hazard
count_df <- as.data.frame(table(filtered_data_without_none$Carriageway_Hazards))
colnames(count_df) <- c("Carriageway_Hazards", "Count")

p1 <- ggplot(filtered_data_without_none, aes(x = Carriageway_Hazards)) +
  geom_bar(aes(y = ..count..), fill = "skyblue") +
  labs(x = "Небезпеки проїжджої частини", y = "Кількість", fill="Серйозність аварії") +
  geom_text(data = count_df, aes(label = Count, y = Count), vjust = -0.5)

p2 <- ggplot(filtered_data_without_none, aes(x = Carriageway_Hazards, fill = Accident_Severity)) +
  geom_bar() +
  labs(x = "Carriageway Hazards", y = "Count", fill = "Accident Severity")


p_combined <- p1 +
  geom_bar(data = filtered_data_without_none, aes(x = Carriageway_Hazards, fill = Accident_Severity), position = "dodge") +
  scale_fill_manual(values = c("lightcoral", "green", "deepskyblue"), labels = c("Фатальна", "Серйозна", "Легка")) +
  scale_x_discrete(labels = c("Будь-яка тварина \n на проїжджій частині \n (крім будинку для \n верхової їзди)", "Інший об'єкт \n на дорозі", "Пішохід на \n проїжджій частині \n - не постраждав", "Попередня аварія", "Навантаження автомобіля \n на дорозі")) +
  theme(legend.position = "top")

print(p_combined)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Графік 5.7. (Слайд 24). Діаграма наявності перепони на дорожньому шляху, лише з тих випадків, де перепона була

carriageway_hazard_counts <- filtered_data_3 %>%
  group_by(Carriageway_Hazards) %>%
  summarise(count = n())
carriageway_hazard_nonone <- carriageway_hazard_counts %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(Carriageway_Hazards != "None")


carriageway_hazard_nonone$Carriageway_Hazards <- ifelse(carriageway_hazard_nonone$Carriageway_Hazards == "Any animal in carriageway (except ridden horse)",
                                                        "Any animal in carriageway",
                                                        carriageway_hazard_nonone$Carriageway_Hazards)

carriageway_hazard_histogram <- ggplot(carriageway_hazard_nonone, aes(x = Carriageway_Hazards, y = percentage, fill = Carriageway_Hazards)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 2), "%")), vjust = -0.5, size = 3, color = "black") +
  scale_x_discrete(labels = c("Будь-яка тварина \n на проїжджій частині \n (крім будинку для \n верхової їзди)", "Інший об'єкт \n на дорозі", "Пішохід на \n проїжджій частині \n - не постраждав", "Попередня аварія", "Навантаження автомобіля \n на дорозі")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),legend.position = "none") +
  labs(title = "Розподіл небезпек на проїжджій частині", x = "Небезпеки проїжджої частини", y = "Відсоток усіх аварій")

print(carriageway_hazard_histogram)
