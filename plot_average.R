# Загрузка библиотеки dplyr
library(dplyr)

# Создаем вектор country_names
# Загрузка библиотеки dplyr (если потребуется для дальнейших операций)
library(dplyr)

# Создаем вектор country_names (91 страна)
country_names <- c("Albania", "Azerbaijan", "Austria", "Armenia", "Bosnia and Herzegovina",
                   "Bulgaria", "Belarus", "Croatia", "Czech Republic", "Denmark", "Estonia",
                   "Finland", "France", "Georgia", "Germany", "Hungary", "Iceland", "Italy",
                   "Latvia", "Lithuania", "Montenegro", "Netherlands", "Norway", "Poland",
                   "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain",
                   "Sweden", "Switzerland", "Ukraine", "North Macedonia", "United Kingdom",
                   "Andorra", "Argentina", "Australia", "Bangladesh", "Bolivia", "Brazil",
                   "Canada", "Colombia", "Cyprus", "Chile", "China", "Ecuador", "Egypt",
                   "Ethiopia", "Greece", "Guatemala", "Hong Kong", "India", "Indonesia", "Iran",
                   "Iraq", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kyrgyzstan", "Lebanon",
                   "Libya", "Macao", "Malaysia", "Maldives", "Mexico", "Mongolia", "Morocco",
                   "Myanmar", "New Zealand", "Nicaragua", "Nigeria",
                   "Pakistan", "Peru", "Philippines", "Puerto Rico", "Singapore", "South Korea",
                   "Taiwan", "Tajikistan", "Thailand", "Tunisia", "Turkey", "United States",
                   "Uruguay", "Uzbekistan", "Venezuela", "Vietnam", "Zimbabwe")

# Создаем вектор cntry_AN (91 страна)
cntry_AN <- c("AL", "AZ", "AT", "AM", "BA", "BG", "BY", "HR", "CZ", "DK", "EE", "FI", "FR",
              "GE", "DE", "HU", "IS", "IT", "LV", "LT", "ME", "NL", "NO", "PL", "PT", "RO", "RU",
              "RS", "SK", "SI", "ES", "SE", "CH", "UA", "MK", "GB", "AD", "AR", "AU", "BD",
              "BO", "BR", "CA", "CO", "CY", "CL", "CN", "EC", "EG", "ET", "GR", "GT", "HK",
              "IN", "ID", "IR", "IQ", "JP", "JO", "KZ", "KE", "KG", "LB", "LY", "MO", "MY",
              "MV", "MX", "MN", "MA", "MM", "NZ", "NI", "NG", "PK", "PE", "PH", "PR",
              "SG", "KR", "TW", "TJ", "TH", "TN", "TR", "US", "UY", "UZ", "VE", "VN", "ZW")

# Создаем датафрейм с трехбуквенными кодами (91 страна)
three_letter_codes <- data.frame(
  cntry_AN = cntry_AN,
  cntry_AN3 = c("ALB", "AZE", "AUT", "ARM", "BIH", "BGR", "BLR", "HRV", "CZE", "DNK", "EST", "FIN", "FRA",
                "GEO", "DEU", "HUN", "ISL", "ITA", "LVA", "LTU", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS",
                "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "MKD", "GBR", "AND", "ARG", "AUS", "BGD",
                "BOL", "BRA", "CAN", "COL", "CYP", "CHL", "CHN", "ECU", "EGY", "ETH", "GRC", "GTM", "HKG",
                "IND", "IDN", "IRN", "IRQ", "JPN", "JOR", "KAZ", "KEN", "KGZ", "LBN", "LBY", "MAC", "MYS",
                "MDV", "MEX", "MNG", "MAR", "MMR", "NZL", "NIC", "NGA", "PAK", "PER", "PHL", "PRI",
                "SGP", "KOR", "TWN", "TJK", "THA", "TUN", "TUR", "USA", "URY", "UZB", "VEN", "VNM", "ZWE")
)

# Проверяем, что датафрейм создан корректно
print(dim(three_letter_codes))  # Должно вывести [91, 2]
print(head(three_letter_codes)) # Первые несколько строк для проверки

# Создаем датафрейм для маппинга имен стран
country_map <- data.frame(cntry_AN = cntry_AN, Country_Name_map = country_names)


# Удаляем дублирующиеся столбцы
wvs_evs_2017 <- wvs_evs_2017[, !duplicated(colnames(wvs_evs_2017))]

# Проверяем наличие дублирующихся столбцов
duplicated_cols <- colnames(wvs_evs_2017)[duplicated(colnames(wvs_evs_2017))]
if (length(duplicated_cols) > 0) {
  stop("Дублирующиеся столбцы все еще присутствуют: ", paste(duplicated_cols, collapse = ", "))
}

# Переименовываем столбец Country_Name, если он существует
if ("Country_Name" %in% colnames(wvs_evs_2017)) {
  wvs_evs_2017 <- wvs_evs_2017 %>% rename(Country_Name_orig = Country_Name)
} else {
  message("Столбец 'Country_Name' не найден в датафрейме")
}

# Объединяем country_map и three_letter_codes в одну таблицу
combined_map <- merge(country_map, three_letter_codes, by = "cntry_AN", all = TRUE)

# Выполняем слияние с wvs_evs_2017
wvs_evs_2017 <- merge(wvs_evs_2017, combined_map, by = "cntry_AN", all.x = TRUE)

# Проверяем имена столбцов
colnames(wvs_evs_2017)

# Выполняем группировку и агрегацию
an_wvs_evs_2017 <- wvs_evs_2017 %>% 
  group_by(cntry_AN3) %>%
  summarise_all(mean, na.rm = TRUE)

# Стандартизация переменных
an_wvs_evs_2017$index_pat_std <- scale(an_wvs_evs_2017$index_pat)


# Calculate Pearson correlation and p-value for standardized variables
corr_test <- cor.test(an_wvs_evs_2017$index_pat_std, an_wvs_evs_2017$abortion, method = "pearson")
corr_value <- round(corr_test$estimate, 3) # Round correlation to 3 decimal places
p_value <- corr_test$p.value < 0.05 # Check if p < 0.05
p_text <- ifelse(p_value, "p < 0.05", "p ≥ 0.05") # Simplified significance text

# Create annotation text
corr_text <- paste("r = ", corr_value, "\n", p_text, sep = "")

# График с использованием стандартизированных переменных
ggplot(an_wvs_evs_2017, aes(x = index_pat_std, y = abortion)) +
  geom_point(size = 3, color = "#1b9e77", alpha = 0.8) + 
  geom_text_repel(aes(label = cntry_AN3), size = 3.5, max.overlaps = 20, 
                  box.padding = 0.5, point.padding = 0.3, segment.color = "grey50") +
  geom_smooth(method = "lm", color = "#d95f02", se = TRUE, fill = "#d95f02", alpha = 0.2) +
  labs(
    x = "Индекс патримониализма (Стандартизованный)",
    y = "Усредненное отношение к абортам"
  ) +
  annotate("text", x = Inf, y = Inf, label = corr_text, hjust = 1.1, vjust = 1.1, 
           size = 4, color = "black", fontface = "bold") +  # жирный текст корреляции
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)), # жирный X
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)), # жирный Y
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks.x = element_line(color = "black", size = 0.5),
    axis.ticks.y = element_line(color = "black", size = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
