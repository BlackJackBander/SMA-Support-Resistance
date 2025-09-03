# Загрузка библиотек
library(quantmod)
library(ggplot2)
library(dplyr)
library(zoo)
library(TTR)
library(purrr)

# Функция для анализа отскоков с логарифмированной ценой
analyze_ma_period_log <- function(data, ma_period, touch_threshold = 0.5) {
  
  # Добавляем логарифмированную цену
  data$LogClose <- log(data$Close)
  
  # Вычисляем скользящую среднюю на логарифмированной цене
  data$MA_Log <- rollmean(data$LogClose, k = ma_period, fill = NA, align = "right")
  
  # Пропускаем первые дни, где MA не определена
  data_clean <- data[!is.na(data$MA_Log), ]
  
  # Определяем близость логарифмированной цены к скользящей средней
  data_clean$LogDistance <- abs(data_clean$LogClose - data_clean$MA_Log)
  data_clean$Touch <- data_clean$LogDistance <= (touch_threshold / 100) # преобразуем % в лог-единицы
  
  # Определяем направление тренда
  data_clean$Trend <- ifelse(data_clean$LogClose > data_clean$MA_Log, "Above", "Below")
  
  # Инициализируем счетчики
  total_touches <- sum(data_clean$Touch, na.rm = TRUE)
  successful_bounces <- 0
  bounce_details <- list()
  
  # Анализируем каждый день для определения успешных отскоков
  for (i in 3:(nrow(data_clean)-5)) {
    if (data_clean$Touch[i] && !is.na(data_clean$Trend[i])) {
      current_trend <- data_clean$Trend[i]
      current_log_price <- data_clean$LogClose[i]
      current_price <- data_clean$Close[i]
      
      # Проверяем отскок в течение следующих 5 дней
      future_log_prices <- data_clean$LogClose[(i+1):(i+5)]
      future_prices <- data_clean$Close[(i+1):(i+5)]
      
      if (current_trend == "Below") {
        # Отскок вверх: цена была ниже MA и пошла вверх
        price_changes <- exp(future_log_prices - current_log_price) - 1
        if (any(price_changes > 0.01)) { # +1% рост в обычных единицах
          successful_bounces <- successful_bounces + 1
          max_change <- max(price_changes, na.rm = TRUE)
          bounce_details[[length(bounce_details) + 1]] <- data.frame(
            Date = data_clean$Date[i],
            Type = "Upward",
            PriceChange = max_change * 100
          )
        }
      } else {
        # Отскок вниз: цена была выше MA и пошла вниз
        price_changes <- 1 - exp(future_log_prices - current_log_price)
        if (any(price_changes > 0.01)) { # -1% падение в обычных единицах
          successful_bounces <- successful_bounces + 1
          max_change <- max(price_changes, na.rm = TRUE)
          bounce_details[[length(bounce_details) + 1]] <- data.frame(
            Date = data_clean$Date[i],
            Type = "Downward",
            PriceChange = max_change * 100
          )
        }
      }
    }
  }
  
  # Рассчитываем процент успешных отскоков
  bounce_rate <- ifelse(total_touches > 0, 
                        (successful_bounces / total_touches) * 100, 
                        0)
  
  # Собираем детали отскоков
  bounce_details_df <- if (length(bounce_details) > 0) bind_rows(bounce_details) else NULL
  
  return(list(
    summary = data.frame(
      MA_Period = ma_period,
      Total_Touches = total_touches,
      Successful_Bounces = successful_bounces,
      Bounce_Rate = bounce_rate,
      Avg_Bounce_Strength = if (!is.null(bounce_details_df)) mean(bounce_details_df$PriceChange) else NA
    ),
    details = bounce_details_df
  ))
}

# Получаем данные по акциям MSFT за 3 года
getSymbols("MSFT", from = Sys.Date() - 365*3, to = Sys.Date())

# Создаем dataframe с данными
msft_data <- data.frame(Date = index(MSFT), 
                        Close = as.numeric(Cl(MSFT)),
                        LogClose = log(as.numeric(Cl(MSFT))))

# Тестируем разные периоды MA (от 10 до 100 дней)
ma_periods <- seq(10, 100, by = 2)

# Анализируем каждый период MA
results_list <- map(ma_periods, ~analyze_ma_period_log(msft_data, .x))

# Извлекаем summary результаты
results <- map_df(results_list, ~.x$summary)

# Находим оптимальный период по проценту отскоков
optimal_period_rate <- results[which.max(results$Bounce_Rate), ]

# Находим оптимальный период по силе отскоков
optimal_period_strength <- results[which.max(results$Avg_Bounce_Strength), ]

# Выводим результаты
cat("Результаты анализа оптимального периода MA (логарифмическая цена):\n")
print(results)

cat("\n=== ОПТИМАЛЬНЫЙ ПЕРИОД ПО ПРОЦЕНТУ ОТСКОКОВ ===\n")
cat("Период MA:", optimal_period_rate$MA_Period, "дней\n")
cat("Всего касаний:", optimal_period_rate$Total_Touches, "\n")
cat("Успешных отскоков:", optimal_period_rate$Successful_Bounces, "\n")
cat("Процент отскоков:", round(optimal_period_rate$Bounce_Rate, 2), "%\n")
cat("Средняя сила отскока:", round(optimal_period_rate$Avg_Bounce_Strength, 2), "%\n")

cat("\n=== ОПТИМАЛЬНЫЙ ПЕРИОД ПО СИЛЕ ОТСКОКОВ ===\n")
cat("Период MA:", optimal_period_strength$MA_Period, "дней\n")
cat("Процент отскоков:", round(optimal_period_strength$Bounce_Rate, 2), "%\n")
cat("Средняя сила отскока:", round(optimal_period_strength$Avg_Bounce_Strength, 2), "%\n")

# Визуализация результатов
p1 <- ggplot(results, aes(x = MA_Period, y = Bounce_Rate)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "red", size = 2) +
  geom_point(data = optimal_period_rate, color = "green", size = 4) +
  geom_point(data = optimal_period_strength, color = "orange", size = 4) +
  geom_text(data = optimal_period_rate, 
            aes(label = paste("По проценту:", MA_Period, "дн")),
            vjust = -1, hjust = 0.5, color = "darkgreen", size = 3) +
  geom_text(data = optimal_period_strength, 
            aes(label = paste("По силе:", MA_Period, "дн")),
            vjust = 2, hjust = 0.5, color = "darkorange", size = 3) +
  labs(title = "Процент отскоков от периода MA",
       x = "Период скользящей средней (дни)",
       y = "Процент успешных отскоков (%)") +
  theme_minimal()

p2 <- ggplot(results, aes(x = MA_Period, y = Avg_Bounce_Strength)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_point(color = "orange", size = 2) +
  geom_point(data = optimal_period_strength, color = "red", size = 4) +
  labs(title = "Средняя сила отскоков",
       x = "Период скользящей средней (дни)",
       y = "Среднее изменение цены (%)") +
  theme_minimal()

# Комбинируем графики
grid.arrange(p1, p2, ncol = 1)

# Детальный анализ для оптимального периода
cat("\n=== ДЕТАЛЬНЫЙ АНАЛИЗ ДЛЯ ПЕРИОДА", optimal_period_rate$MA_Period, "ДНЕЙ ===\n")
optimal_analysis <- analyze_ma_period_log(msft_data, optimal_period_rate$MA_Period)

cat("Распределение силы отскоков:\n")
if (!is.null(optimal_analysis$details)) {
  print(summary(optimal_analysis$details$PriceChange))
  cat("Медианная сила отскока:", median(optimal_analysis$details$PriceChange), "%\n")
  
  # График распределения силы отскоков
  ggplot(optimal_analysis$details, aes(x = PriceChange, fill = Type)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
    geom_vline(aes(xintercept = mean(PriceChange)), color = "red", linetype = "dashed") +
    labs(title = paste("Распределение силы отскоков для MA", optimal_period_rate$MA_Period, "дней"),
         x = "Изменение цены после отскока (%)",
         y = "Количество отскоков",
         fill = "Тип отскока") +
    theme_minimal() +
    scale_fill_manual(values = c("Upward" = "green", "Downward" = "red"))
}

# Сравнение с обычной ценой (без логарифмирования)
cat("\n=== СРАВНЕНИЕ С ОБЫЧНЫМ РАСЧЕТОМ (без логарифмирования) ===\n")

# Функция для обычного расчета
analyze_ma_period_regular <- function(data, ma_period, touch_threshold = 0.5) {
  data$MA <- rollmean(data$Close, k = ma_period, fill = NA, align = "right")
  data_clean <- data[!is.na(data$MA), ]
  data_clean$Distance <- abs(data_clean$Close - data_clean$MA) / data_clean$MA * 100
  data_clean$Touch <- data_clean$Distance <= touch_threshold
  data_clean$Trend <- ifelse(data_clean$Close > data_clean$MA, "Above", "Below")
  
  total_touches <- sum(data_clean$Touch, na.rm = TRUE)
  successful_bounces <- 0
  
  for (i in 3:(nrow(data_clean)-5)) {
    if (data_clean$Touch[i]) {
      current_trend <- data_clean$Trend[i]
      current_price <- data_clean$Close[i]
      future_prices <- data_clean$Close[(i+1):(i+5)]
      
      if (current_trend == "Below") {
        if (any(future_prices > current_price * 1.01)) {
          successful_bounces <- successful_bounces + 1
        }
      } else {
        if (any(future_prices < current_price * 0.99)) {
          successful_bounces <- successful_bounces + 1
        }
      }
    }
  }
  
  bounce_rate <- ifelse(total_touches > 0, (successful_bounces / total_touches) * 100, 0)
  return(bounce_rate)
}

# Сравниваем результаты
regular_rate <- analyze_ma_period_regular(msft_data, optimal_period_rate$MA_Period)
log_rate <- optimal_period_rate$Bounce_Rate

cat("Процент отскоков (обычный расчет):", round(regular_rate, 2), "%\n")
cat("Процент отскоков (логарифмический):", round(log_rate, 2), "%\n")
cat("Разница:", round(log_rate - regular_rate, 2), "%\n")

# Строим финальный график с оптимальной скользящей средней
optimal_ma_period <- optimal_period_rate$MA_Period

# Вычисляем оптимальную скользящую среднюю
msft_data$Optimal_MA <- rollmean(msft_data$Close, k = optimal_ma_period, fill = NA, align = "right")

# Создаем график
final_plot <- ggplot(msft_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Цена закрытия"), linewidth = 0.8, alpha = 0.9) +
  geom_line(aes(y = Optimal_MA, color = "Оптимальная MA"), linewidth = 1.2, alpha = 0.8) +
  
  # Добавляем зону вокруг MA для визуализации касаний
  geom_ribbon(aes(ymin = Optimal_MA * 0.995, 
                  ymax = Optimal_MA * 1.005,
                  fill = "Зона касания (±0.5%)"), 
              alpha = 0.2) +
  
  scale_color_manual(values = c(
    "Цена закрытия" = "black",
    "Оптимальная MA" = "blue"
  )) +
  
  scale_fill_manual(values = c(
    "Зона касания (±0.5%)" = "yellow"
  )) +
  
  labs(title = paste("Акции MSFT с оптимальной скользящей средней (", optimal_ma_period, " дней)", sep = ""),
       subtitle = paste("Процент отскоков: ", round(optimal_period_rate$Bounce_Rate, 1), "%", 
                        " | Сила отскоков: ", round(optimal_period_rate$Avg_Bounce_Strength, 1), "%", sep = ""),
       x = "Дата", 
       y = "Цена закрытия ($)",
       color = "Линии",
       fill = "Области") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  ) +
  
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2))

# Выводим финальный график
print(final_plot)

# Дополнительная информация о графике
cat("\n=== ФИНАЛЬНЫЙ ГРАФИК ===\n")
cat("Период оптимальной скользящей средней:", optimal_ma_period, "дней\n")
cat("Процент успешных отскоков:", round(optimal_period_rate$Bounce_Rate, 2), "%\n")
cat("Средняя сила отскоков:", round(optimal_period_rate$Avg_Bounce_Strength, 2), "%\n")
cat("Общее количество точек данных:", sum(!is.na(msft_data$Optimal_MA)), "\n")
cat("Период данных: с", format(min(msft_data$Date), "%Y-%m-%d"), 
    "по", format(max(msft_data$Date), "%Y-%m-%d"), "\n")

# Показываем статистику по ценам
cat("\nСтатистика цен за период:\n")
cat("Минимальная цена: $", round(min(msft_data$Close, na.rm = TRUE), 2), "\n")
cat("Максимальная цена: $", round(max(msft_data$Close, na.rm = TRUE), 2), "\n")
cat("Средняя цена: $", round(mean(msft_data$Close, na.rm = TRUE), 2), "\n")
cat("Текущая цена: $", round(msft_data$Close[nrow(msft_data)], 2), "\n")

# Сохраняем график в файл
ggsave(paste0("MSFT_Optimal_MA_", optimal_ma_period, "_days.png"), 
       final_plot, width = 12, height = 8, dpi = 300)

cat("\nГрафик сохранен в файл: MSFT_Optimal_MA_", optimal_ma_period, "_days.png\n", sep = "")

# Дополнительный график с детализацией последнего года
last_year_data <- msft_data[msft_data$Date >= Sys.Date() - 365, ]

last_year_plot <- ggplot(last_year_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Цена закрытия"), linewidth = 0.8) +
  geom_line(aes(y = Optimal_MA, color = "Оптимальная MA"), linewidth = 1.2) +
  geom_ribbon(aes(ymin = Optimal_MA * 0.995, 
                  ymax = Optimal_MA * 1.005,
                  fill = "Зона касания"), 
              alpha = 0.2) +
  
  scale_color_manual(values = c(
    "Цена закрытия" = "black",
    "Оптимальная MA" = "blue"
  )) +
  
  labs(title = paste("Детализация последнего года (MA ", optimal_ma_period, " дней)", sep = ""),
       x = "Дата", y = "Цена закрытия ($)") +
  
  theme_minimal() +
  theme(legend.position = "bottom")

print(last_year_plot)

cat("\n=== ДОПОЛНИТЕЛЬНАЯ ИНФОРМАЦИЯ ===\n")
cat("Количество торговых дней с полными данными:", sum(!is.na(msft_data$Optimal_MA)), "\n")
cat("Первая дата с полными данными:", format(msft_data$Date[which(!is.na(msft_data$Optimal_MA))[1]], "%Y-%m-%d"), "\n")

# Показываем несколько последних значений
cat("\nПоследние 5 значений цены и MA:\n")
last_values <- tail(msft_data[!is.na(msft_data$Optimal_MA), c("Date", "Close", "Optimal_MA")], 5)
last_values$Difference <- last_values$Close - last_values$Optimal_MA
last_values$Difference_Pct <- (last_values$Difference / last_values$Optimal_MA) * 100
print(last_values)

# Анализ текущего положения относительно MA
current_diff <- last_values$Difference[5]
current_diff_pct <- last_values$Difference_Pct[5]

cat("\nТекущее положение относительно MA:\n")
if (current_diff > 0) {
  cat("Цена НАД MA на ", round(current_diff, 2), " ($", round(current_diff_pct, 2), "%)\n", sep = "")
} else if (current_diff < 0) {
  cat("Цена ПОД MA на ", round(abs(current_diff), 2), " ($", round(abs(current_diff_pct), 2), "%)\n", sep = "")
} else {
  cat("Цена НА MA\n")
}

cat("Расстояние до зоны касания: ", round(abs(current_diff_pct) - 0.5, 2), "%\n", sep = "")


# Функция для анализа с определением максимального отскока
analyze_ma_period_with_max_bounce <- function(data, ma_period, touch_threshold = 0.5) {
  
  # Добавляем логарифмированную цену
  data$LogClose <- log(data$Close)
  
  # Вычисляем скользящую среднюю на логарифмированной цене
  data$MA_Log <- rollmean(data$LogClose, k = ma_period, fill = NA, align = "right")
  
  # Пропускаем первые дни, где MA не определена
  data_clean <- data[!is.na(data$MA_Log), ]
  
  # Определяем близость логарифмированной цены к скользящей средней
  data_clean$LogDistance <- abs(data_clean$LogClose - data_clean$MA_Log)
  data_clean$Touch <- data_clean$LogDistance <= (touch_threshold / 100)
  
  # Определяем направление тренда
  data_clean$Trend <- ifelse(data_clean$LogClose > data_clean$MA_Log, "Above", "Below")
  
  # Инициализируем счетчики
  total_touches <- sum(data_clean$Touch, na.rm = TRUE)
  successful_bounces <- 0
  bounce_details <- list()
  max_upward_bounce <- 0
  max_downward_bounce <- 0
  max_upward_date <- NA
  max_downward_date <- NA
  
  # Анализируем каждый день для определения успешных отскоков
  for (i in 3:(nrow(data_clean)-5)) {
    if (data_clean$Touch[i] && !is.na(data_clean$Trend[i])) {
      current_trend <- data_clean$Trend[i]
      current_log_price <- data_clean$LogClose[i]
      current_date <- data_clean$Date[i]
      current_price <- data_clean$Close[i]
      
      # Проверяем отскок в течение следующих 10 дней
      lookahead_days <- min(10, nrow(data_clean) - i)
      future_log_prices <- data_clean$LogClose[(i+1):(i+lookahead_days)]
      future_prices <- data_clean$Close[(i+1):(i+lookahead_days)]
      future_dates <- data_clean$Date[(i+1):(i+lookahead_days)]
      
      if (current_trend == "Below") {
        # Отскок вверх: цена была ниже MA и пошла вверх
        price_changes <- exp(future_log_prices - current_log_price) - 1
        max_change <- max(price_changes, na.rm = TRUE)
        max_change_index <- which.max(price_changes)
        
        if (max_change > 0.01) {
          successful_bounces <- successful_bounces + 1
          bounce_details[[length(bounce_details) + 1]] <- data.frame(
            Date = current_date,
            Type = "Upward",
            PriceChange = max_change * 100,
            DaysToMax = max_change_index,
            MaxChangeDate = future_dates[max_change_index]
          )
          
          # Обновляем максимальный отскок вверх
          if (max_change * 100 > max_upward_bounce) {
            max_upward_bounce <- max_change * 100
            max_upward_date <- current_date
          }
        }
      } else {
        # Отскок вниз: цена была выше MA и пошла вниз
        price_changes <- 1 - exp(future_log_prices - current_log_price)
        max_change <- max(price_changes, na.rm = TRUE)
        max_change_index <- which.max(price_changes)
        
        if (max_change > 0.01) {
          successful_bounces <- successful_bounces + 1
          bounce_details[[length(bounce_details) + 1]] <- data.frame(
            Date = current_date,
            Type = "Downward",
            PriceChange = max_change * 100,
            DaysToMax = max_change_index,
            MaxChangeDate = future_dates[max_change_index]
          )
          
          # Обновляем максимальный отскок вниз
          if (max_change * 100 > max_downward_bounce) {
            max_downward_bounce <- max_change * 100
            max_downward_date <- current_date
          }
        }
      }
    }
  }
  
  # Рассчитываем процент успешных отскоков
  bounce_rate <- ifelse(total_touches > 0, (successful_bounces / total_touches) * 100, 0)
  
  # Собираем детали отскоков
  bounce_details_df <- if (length(bounce_details) > 0) bind_rows(bounce_details) else NULL
  
  return(list(
    summary = data.frame(
      MA_Period = ma_period,
      Total_Touches = total_touches,
      Successful_Bounces = successful_bounces,
      Bounce_Rate = bounce_rate,
      Avg_Bounce_Strength = if (!is.null(bounce_details_df)) mean(bounce_details_df$PriceChange) else NA,
      Max_Upward_Bounce = max_upward_bounce,
      Max_Downward_Bounce = max_downward_bounce
    ),
    details = bounce_details_df,
    max_bounces = data.frame(
      Max_Upward_Date = max_upward_date,
      Max_Upward_Percent = max_upward_bounce,
      Max_Downward_Date = max_downward_date,
      Max_Downward_Percent = max_downward_bounce
    )
  ))
}

# Анализируем с максимальными отскоками
results_list_max <- map(ma_periods, ~analyze_ma_period_with_max_bounce(msft_data, .x))
results_max <- map_df(results_list_max, ~.x$summary)

# Находим оптимальный период по максимальному отскоку
optimal_period_max_up <- results_max[which.max(results_max$Max_Upward_Bounce), ]
optimal_period_max_down <- results_max[which.max(results_max$Max_Downward_Bounce), ]

# Выводим результаты максимальных отскоков
cat("\n=== МАКСИМАЛЬНЫЕ ОТСКОКИ ===\n")

cat("МАКСИМАЛЬНЫЙ ОТСКОК ВВЕРХ:\n")
cat("Период MA:", optimal_period_max_up$MA_Period, "дней\n")
cat("Процент отскока:", round(optimal_period_max_up$Max_Upward_Bounce, 2), "%\n")
cat("Процент успешных отскоков:", round(optimal_period_max_up$Bounce_Rate, 2), "%\n")

cat("\nМАКСИМАЛЬНЫЙ ОТСКОК ВНИЗ:\n")
cat("Период MA:", optimal_period_max_down$MA_Period, "дней\n")
cat("Процент отскока:", round(optimal_period_max_down$Max_Downward_Bounce, 2), "%\n")
cat("Процент успешных отскоков:", round(optimal_period_max_down$Bounce_Rate, 2), "%\n")

# Детальная информация о максимальных отскоках для оптимального периода
cat("\n=== ДЕТАЛИ МАКСИМАЛЬНЫХ ОТСКОКОВ ДЛЯ ОПТИМАЛЬНОГО ПЕРИОДА ===\n")

optimal_max_details <- analyze_ma_period_with_max_bounce(msft_data, optimal_period_rate$MA_Period)

cat("Для периода MA", optimal_period_rate$MA_Period, "дней:\n")
cat("Максимальный отскок вверх:", round(optimal_max_details$max_bounces$Max_Upward_Percent, 2), "%\n")
cat("Максимальный отскок вниз:", round(optimal_max_details$max_bounces$Max_Downward_Percent, 2), "%\n")

# Визуализация максимальных отскоков
p3 <- ggplot(results_max, aes(x = MA_Period, y = Max_Upward_Bounce)) +
  geom_line(color = "green", linewidth = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  geom_point(data = optimal_period_max_up, color = "limegreen", size = 4) +
  geom_text(data = optimal_period_max_up, 
            aes(label = paste("Макс:", round(Max_Upward_Bounce, 1), "%")),
            vjust = -1, hjust = 0.5, color = "darkgreen", size = 3) +
  labs(title = "Максимальный отскок вверх",
       x = "Период MA (дни)",
       y = "Максимальный отскок (%)") +
  theme_minimal()

p4 <- ggplot(results_max, aes(x = MA_Period, y = Max_Downward_Bounce)) +
  geom_line(color = "red", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  geom_point(data = optimal_period_max_down, color = "orange", size = 4) +
  geom_text(data = optimal_period_max_down, 
            aes(label = paste("Макс:", round(Max_Downward_Bounce, 1), "%")),
            vjust = -1, hjust = 0.5, color = "darkred", size = 3) +
  labs(title = "Максимальный отскок вниз",
       x = "Период MA (дни)",
       y = "Максимальный отскок (%)") +
  theme_minimal()

# Комбинируем все графики
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Анализ распределения силы отскоков для оптимального периода
if (!is.null(optimal_max_details$details)) {
  cat("\n=== РАСПРЕДЕЛЕНИЕ СИЛЫ ОТСКОКОВ ===\n")
  
  upward_bounces <- optimal_max_details$details %>% filter(Type == "Upward")
  downward_bounces <- optimal_max_details$details %>% filter(Type == "Downward")
  
  cat("Отскоки вверх (", nrow(upward_bounces), " событий):\n", sep = "")
  cat("  Средняя сила:", round(mean(upward_bounces$PriceChange), 2), "%\n")
  cat("  Медианная сила:", round(median(upward_bounces$PriceChange), 2), "%\n")
  cat("  Максимальная сила:", round(max(upward_bounces$PriceChange), 2), "%\n")
  cat("  Минимальная сила:", round(min(upward_bounces$PriceChange), 2), "%\n")
  
  cat("\nОтскоки вниз (", nrow(downward_bounces), " событий):\n", sep = "")
  cat("  Средняя сила:", round(mean(downward_bounces$PriceChange), 2), "%\n")
  cat("  Медианная сила:", round(median(downward_bounces$PriceChange), 2), "%\n")
  cat("  Максимальная сила:", round(max(downward_bounces$PriceChange), 2), "%\n")
  cat("  Минимальная сила:", round(min(downward_bounces$PriceChange), 2), "%\n")
  
  # График распределения силы отскоков
  bounce_distribution <- ggplot(optimal_max_details$details, aes(x = PriceChange, fill = Type)) +
    geom_density(alpha = 0.6) +
    geom_vline(data = data.frame(Type = "Upward", Value = mean(upward_bounces$PriceChange)), 
               aes(xintercept = Value), color = "green", linetype = "dashed") +
    geom_vline(data = data.frame(Type = "Downward", Value = mean(downward_bounces$PriceChange)), 
               aes(xintercept = Value), color = "red", linetype = "dashed") +
    labs(title = paste("Распределение силы отскоков для MA", optimal_period_rate$MA_Period, "дней"),
         x = "Сила отскока (%)",
         y = "Плотность",
         fill = "Тип отскока") +
    theme_minimal() +
    scale_fill_manual(values = c("Upward" = "green", "Downward" = "red"))
  
  print(bounce_distribution)
}

# Топ-5 самых сильных отскоков
cat("\n=== ТОП-5 САМЫХ СИЛЬНЫХ ОТСКОКОВ ===\n")

if (!is.null(optimal_max_details$details)) {
  top_bounces <- optimal_max_details$details %>%
    arrange(desc(PriceChange)) %>%
    head(5)
  
  print(top_bounces)
  
  # Визуализация топ отскоков на основном графике
  final_plot_with_top_bounces <- final_plot +
    geom_point(data = top_bounces, 
               aes(x = Date, y = msft_data$Close[match(as.Date(Date), as.Date(msft_data$Date))]),
               color = "purple", size = 4, shape = 18) +
    geom_text(data = top_bounces,
              aes(x = Date, y = msft_data$Close[match(as.Date(Date), as.Date(msft_data$Date))] * 1.03,
                  label = paste(round(PriceChange, 1), "%")),
              color = "purple", size = 3, fontface = "bold") +
    labs(subtitle = paste("Процент отскоков: ", round(optimal_period_rate$Bounce_Rate, 1), "%", 
                          " | Сила отскоков: ", round(optimal_period_rate$Avg_Bounce_Strength, 1), "%",
                          " | Макс. отскок: ", round(max(optimal_max_details$details$PriceChange), 1), "%", sep = ""))
  
  print(final_plot_with_top_bounces)
}

# Сводная таблица по всем метрикам
cat("\n=== СВОДНАЯ ТАБЛИЦА ОПТИМАЛЬНЫХ ПЕРИОДОВ ===\n")
summary_table <- data.frame(
  Метрика = c("Процент отскоков", "Сила отскоков", "Макс. отскок вверх", "Макс. отскок вниз"),
  Период_MA = c(optimal_period_rate$MA_Period, 
                optimal_period_strength$MA_Period,
                optimal_period_max_up$MA_Period,
                optimal_period_max_down$MA_Period),
  Значение = c(paste(round(optimal_period_rate$Bounce_Rate, 1), "%"),
               paste(round(optimal_period_strength$Avg_Bounce_Strength, 1), "%"),
               paste(round(optimal_period_max_up$Max_Upward_Bounce, 1), "%"),
               paste(round(optimal_period_max_down$Max_Downward_Bounce, 1), "%"))
)

print(summary_table)
