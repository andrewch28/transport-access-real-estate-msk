Диплом
================
Chertkov Andrei
2025-02-19

``` r
#install.packages("car") 
#install.packages(knitr)
#install.packages("MatchIt")
#install.packages("lmtest")
#install.packages("cobalt")
#install.packages("grf")
#install.packages("WeightIt")
#install.packages("lubridate")
#install.packages("tableone")
#install.packages("RItools")
#install.packages("modelsummary")
#install.packages("flextable")
#install.packages("officer")
```

## Загружаем данные

``` r
prematched_df <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE) 

prematched_df <- prematched_df %>%
  mutate(treatment = ifelse(`Сокращение.расстояния` > 1000, 1, 0))
```

## Базовая разность разностей

### Оба воздействия

``` r
prematched_df <- prematched_df %>%
  mutate(РУДН = ifelse(`Название.первой.станции` == "Университет Дружбы Народов", 1, 0)) %>%
  mutate(Тютчевская = ifelse(`Название.первой.станции` == "Тютчевская", 1, 0)) %>%
  mutate(Генерала_тюленева = ifelse(`Название.первой.станции` == "Генерала Тюленева", 1, 0))

prematched_df <- prematched_df %>%
  mutate(
    year_2020 = ifelse(format(as.Date(reportDate), "%Y") == "2020", 1, 0),
    year_2021 = ifelse(format(as.Date(reportDate), "%Y") == "2021", 1, 0),
    year_2022 = ifelse(format(as.Date(reportDate), "%Y") == "2022", 1, 0),
    year_2023 = ifelse(format(as.Date(reportDate), "%Y") == "2023", 1, 0),
    year_2024 = ifelse(format(as.Date(reportDate), "%Y") == "2024", 1, 0),
    year_2025 = ifelse(format(as.Date(reportDate), "%Y") == "2025", 1, 0),
  )


prematched_df <- prematched_df %>% 
    rename( 
          Год_постройки = Год.постройки, 
          Количество_квартир = Квартир, 
          Высота_потолков = Высота.потолков, 
          Спортивная_площадка = Спортивная.площадка.binary, 
          Детская_площадка = Детская.площадка.binary, 
          Газоснабжение = Газоснабжение_dummy, 
          Пассажирских_лифтов = Пассажирских.на.подъезд,
          Грузовых_лифтов = Грузовых.на.подъезд,
          PCA_компонента = PCA_Component_correct)
```

``` r
prematched_df <- prematched_df %>%
  mutate(opening = ifelse(`reportDate` > '2024-09-01', 1, 0))

prematched_df <- prematched_df %>%
  mutate(announcement = ifelse(`reportDate` > '2022-10-01', 1, 0))


model_full <- lm(log(discounted_value) ~ treatment + opening + announcement + treatment * opening + treatment * announcement + Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            data = prematched_df
            )

summary(model_full)
```

``` r
confint(model_full, level = 0.95)
```

``` r
vif(model_full, type="predictor")
```

### Только открытие

``` r
model_opening <- lm(log(discounted_value) ~ treatment + opening + treatment * opening +
Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            data = prematched_df)
summary(model_opening)
```

``` r
vif(model_opening, type = "predictor")
```

### Только объявление

``` r
model_announcement <- lm(log(discounted_value) ~ treatment + announcement + treatment * announcement + Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            data = prematched_df)
          #  weights = weights)

summary(model_announcement)
```

## Модели с мэтчингом

### Проверяем необходимость в мэтчинге

``` r
table_NO_matching <- CreateTableOne(vars=c("Год_постройки", "Этажность", "Высота_потолков", 
                "Количество_квартир", "Спортивная_площадка", "Детская_площадка", "Газоснабжение", "Пассажирских_лифтов", "Грузовых_лифтов", "PCA_Component"), 
                         strata = 'treatment', data=prematched_df, test=TRUE)
table_NO_matching
```

``` r
#table1_df <- print(table1, printToggle = FALSE)
#table2_df <- print(table2, printToggle = FALSE)
#write.csv(table1_df, "table1_summary.csv", row.names = TRUE)
#write.csv(table2_df, "table2_summary.csv", row.names = TRUE)
```

``` r
xBalance(treatment ~ Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_компонента, 
               data = prematched_df, report = 'chisquare.test')
```

### Разность разность с мэтчингом

``` r
match_model <- matchit(treatment ~ Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component,
                        data = prematched_df, 
                        method = "nearest", 
                       distance = "logit",
                        ratio=5)  
```

``` r
summary(match_model)
```

``` r
matched_data <- match.data(match_model)
love.plot(match_model)
```

#### Обе переменные взаимодействия

``` r
model_full_matched <- lm(log(discounted_value) ~ treatment + opening + announcement + treatment * opening + treatment * announcement + Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            data = matched_data,
            weights = weights
            )

summary(model_full_matched)
```

#### Только открытие

``` r
model_opening_matched <- lm(log(discounted_value) ~ treatment + opening + treatment * opening +
Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,

            data = matched_data,
            weights = weights)

summary(model_opening_matched)
```

#### Только объявление

``` r
model_announcement_matched <- lm(log(discounted_value) ~ treatment + announcement + treatment * announcement + Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            
            data = matched_data,
           weights = weights)

summary(model_announcement_matched)
```

### Разность разностей с мэтчингом + caliper

``` r
match_model_caliper <- matchit(treatment ~ Год_постройки + Этажность + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component,
                        data = prematched_df, 
                        method = "nearest",
                         distance="logit",
                        #distan10e = "mahalanobis",
                       caliper=0.15,
                        ratio=5)  
```

``` r
summary(match_model_caliper)
#write.csv(summary_result$nn, "matching_summary.csv", row.names = TRUE)
```

``` r
matched_data_caliper <- match.data(match_model_caliper)
love.plot(match_model_caliper)
```

``` r
table_caliper <- CreateTableOne(vars=c("Год_постройки", "Этажность", "Высота_потолков", 
                "Количество_квартир", "Спортивная_площадка", "Детская_площадка", "Газоснабжение", "Пассажирских_лифтов", "Грузовых_лифтов", "PCA_Component"), 
                         strata = 'treatment', data=matched_data_caliper, test=TRUE)
table_caliper
```

#### Обе переменные взаимодействия

``` r
model_full_matched_caliper <- lm(log(discounted_value) ~ treatment + opening + announcement + treatment * opening + treatment * announcement + Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская ,
            data = matched_data_caliper,
            weights = weights
            )

summary(model_full_matched_caliper)
```

``` r
confint(model_announcement_matched_caliper, level = 0.95)
```

#### Только открытие

``` r
model_opening_matched_caliper <- lm(log(discounted_value) ~ treatment + opening + treatment * opening +
Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            data = matched_data_caliper,
            weights = weights)

summary(model_opening_matched_caliper)
```

#### Только объявление

``` r
model_announcement_matched_caliper <- lm(log(discounted_value) ~ treatment + announcement + treatment * announcement + Год_постройки + Этажность  + Количество_квартир + Высота_потолков + Спортивная_площадка + Детская_площадка + Газоснабжение + Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
            
            data = matched_data_caliper,
           weights = weights)

summary(model_announcement_matched_caliper)
```

## Выводим модели

``` r
models_pretrend <- list(
  "Pre-trends check" = pretrend_model
)

ft_pretrend <- modelsummary(
  models_pretrend,
  output = "flextable",
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  fmt = 3,
  stars = c("·" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001)
)

ft_pretrend <- width(ft_pretrend, j = 1, width = 2)
ft_pretrend <- width(ft_pretrend, j = 2, width = 1.5)

ft_pretrend <- border_outer(ft_pretrend, border = fp_border(color = "black", width = 1))
ft_pretrend <- border_inner_h(ft_pretrend, border = fp_border(color = "black", width = 0.75))
ft_pretrend <- border_inner_v(ft_pretrend, border = fp_border(color = "black", width = 0.75))

doc_pretrend <- read_docx()
doc_pretrend <- body_add_par(doc_pretrend, "Проверка параллельности трендов", style = "heading 1")
doc_pretrend <- body_add_flextable(doc_pretrend, ft_pretrend)
print(doc_pretrend, target = "pretrend_check.docx")
```

``` r
models <- list(
  "Diff-in-diff" = model_full,
  "Diff-in-diff opening" = model_opening,
  "Diff-in-diff + announcement" = model_announcement
)

ft <- modelsummary(
  models,
  output = "flextable",
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  fmt = 3,
  stars <- c("·" = 0.1, "*" = 0,05, "**" = 0,01, "***" = 0,001),

)

ft <- width(ft, j = 1, width = 1.5)
ft <- width(ft, j = 2:4, width = 1.5)

ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "black", width = 0.75))
ft <- border_inner_v(ft, border = fp_border(color = "black", width = 0.75))

doc <- read_docx()
doc <- body_add_par(doc, "Результаты регрессионных моделей", style = "heading 1")
doc <- body_add_flextable(doc, ft)
print(doc, target = "full_models.docx")
```

``` r
models <- list(
  "Diff-in-diff matched" = model_full_matched,
  "Diff-in-diff opening matched" = model_opening_matched,
  "Diff-in-diff announcement matched" = model_announcement_matched
)

ft <- modelsummary(
  models,
  output = "flextable",
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  fmt = 3,
  stars <- c("·" = 0.1, "*" = 0,05, "**" = 0,01, "***" = 0,001),
)

ft <- width(ft, j = 1, width = 1.5)
ft <- width(ft, j = 2:4, width = 1.5)

ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "black", width = 0.75))
ft <- border_inner_v(ft, border = fp_border(color = "black", width = 0.75))

doc <- read_docx()
doc <- body_add_par(doc, "Результаты регрессионных моделей", style = "heading 1")
doc <- body_add_flextable(doc, ft)
print(doc, target = "matching.docx")
```

``` r
models <- list(
  "Diff-in-diff caliper" = model_full_matched_caliper,
  "Diff-in-diff opening caliper" = model_opening_matched_caliper,
  "Diff-in-diff announcementcaliper" = model_announcement_matched_caliper
)

ft <- modelsummary(
  models,
  output = "flextable",
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  fmt = 3,
  stars <- c("·" = 0.1, "*" = 0,05, "**" = 0,01, "***" = 0,001),
)

ft <- width(ft, j = 1, width = 1.5)
ft <- width(ft, j = 2:4, width = 1.5)

ft <- border_outer(ft, border = fp_border(color = "black", width = 1))
ft <- border_inner_h(ft, border = fp_border(color = "black", width = 0.75))
ft <- border_inner_v(ft, border = fp_border(color = "black", width = 0.75))

doc <- read_docx()
doc <- body_add_par(doc, "Результаты регрессионных моделей", style = "heading 1")
doc <- body_add_flextable(doc, ft)
print(doc, target = "caliper_matching.docx")
```

## Плацебо тесты

``` r
#prematched_df <- matched_data_caliper

model_real <- lm(log(discounted_value) ~ treatment + opening + announcement + 
                   treatment:opening + treatment:announcement + 
                   Год_постройки + Этажность + Количество_квартир + Высота_потолков + 
                   Спортивная_площадка + Детская_площадка + Газоснабжение + 
                   Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
                 data = prematched_df)
                 #weights = weights)

real_coef <- coef(model_real)["treatment:opening"]

possible_dates <- seq(as.Date("2021-03-01"), as.Date("2024-02-01"), by = "month")

n_iter <- 500
placebo_coefs <- numeric(n_iter)

set.seed(42)

for (i in 1:n_iter) {
  placebo_date <- sample(possible_dates, 1)
  prematched_df$opening_fake <- as.integer(prematched_df$reportDate >= placebo_date)
  prematched_df$treatment_fake <- sample(prematched_df$treatment)
  
  model_placebo <- lm(log(discounted_value) ~ treatment_fake + opening_fake + announcement +
                        treatment_fake:opening_fake + treatment_fake:announcement +
                        Год_постройки + Этажность + Количество_квартир + Высота_потолков +
                        Спортивная_площадка + Детская_площадка + Газоснабжение +
                        Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
                      data = prematched_df)
  print(table(prematched_df$treatment_fake))    
  (table(prematched_df$opening_fake)) 

  placebo_coefs[i] <- coef(model_placebo)["treatment_fake:opening_fake"]
}

dens <- density(placebo_coefs)


mu <- mean(placebo_coefs)
sigma <- sd(placebo_coefs)
x_vals <- seq(min(dens$x), max(dens$x), length.out = 500)
normal_dens <- dnorm(x_vals, mean = mu, sd = sigma)

par(mar = c(4, 4, 2, 1))

plot(NULL,
     xlim = range(dens$x),
     ylim = c(0, max(dens$y, normal_dens ) * 1.1),
    main = "Распределение плацебо-оценок эффекта открытия метро",
     xlab = "Оценка эффекта от открытия",
     ylab = "Плотность вероятности")

lines(dens$x, dens$y, col = "darkgreen", lwd = 2)
lines(x_vals, normal_dens, col = "blue", lty = 2, lwd = 2)


abline(v = real_coef, col = "red", lwd = 2, lty = 2)

legend("topright", legend = paste(round(real_coef, 4)),
       col = "red", lty = 2, lwd = 2)
```

``` r
model_real <- lm(log(discounted_value) ~ treatment + opening + announcement + 
                   treatment:opening + treatment:announcement + 
                   Год_постройки + Этажность + Количество_квартир + Высота_потолков + 
                   Спортивная_площадка + Детская_площадка + Газоснабжение + 
                   Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
                 data = matched_data_caliper,
                 weights = weights)

real_coef_announcement <- coef(model_real)["treatment:announcement"]

possible_dates <- seq(as.Date("2021-03-01"), as.Date("2024-02-01"), by = "month")

n_iter <- 500
placebo_coefs_announcement <- numeric(n_iter)

set.seed(42)

for (i in 1:n_iter) {
  placebo_date <- sample(possible_dates, 1)
  prematched_df$announcement_fake <- as.integer(prematched_df$reportDate >= placebo_date)
  prematched_df$treatment_fake <- sample(prematched_df$treatment)
  
  model_placebo <- lm(log(discounted_value) ~ treatment_fake + opening + announcement_fake +
                        treatment_fake:opening + treatment_fake:announcement_fake +
                        Год_постройки + Этажность + Количество_квартир + Высота_потолков +
                        Спортивная_площадка + Детская_площадка + Газоснабжение +
                        Пассажирских_лифтов + Грузовых_лифтов + PCA_Component + РУДН + Тютчевская,
                      data = prematched_df)
  
  placebo_coefs_announcement[i] <- coef(model_placebo)["treatment_fake:announcement_fake"]
}

mu <- mean(placebo_coefs_announcement)
sigma <- sd(placebo_coefs_announcement)
x_vals <- seq(min(dens_ann$x), max(dens_ann$x), length.out = 500)
normal_dens_an <- dnorm(x_vals, mean = mu, sd = sigma)

dens_ann <- density(placebo_coefs_announcement)  

par(mar = c(4, 4, 2, 1))  
plot(NULL,
     xlim = range(dens_ann$x),
     ylim = c(0, max(dens_ann$y, normal_dens_an) * 1.1),
     xlab = "Эффект от новости о строительстве метро",
     ylab = "Плотность вероятности")

lines(dens_ann$x, dens_ann$y, col = "darkgreen", lwd = 2)
lines(x_vals, normal_dens_an, col = "blue", lty = 2, lwd = 2)


abline(v = real_coef_announcement, col = "red", lwd = 2, lty = 2)

legend("topright", legend = paste("Коэфф. Модели 7 = ", round(real_coef_announcement, 4)),
       col = "red", lty = 2, lwd = 2)
```

## Проверка претрендов

``` r
prematched_df$post <- ifelse(prematched_df$reportDate > as.Date('2024-09-01'), 1, 0)
prematched_df$reportDate <- as.Date(prematched_df$reportDate)
df_pre <- prematched_df %>% filter(post == 0)
df_pre <- df_pre %>%
  mutate(time = as.numeric(difftime(reportDate, min(reportDate), units = "days")))
pretrend_model <- lm(log(discounted_value) ~ time * treatment, data = df_pre)
summary(pretrend_model)
```

## Построение доверительных интервалов

``` r
full_df <- tidy(model_full, conf.int = TRUE) %>%
  filter(term == "treatment:opening") %>%
  mutate(model = "Модель 1")
opening_df <- tidy(model_opening, conf.int = TRUE) %>%
  filter(term == "treatment:opening") %>%
  mutate(model = "Модель 2")
full_matching_df <- tidy(model_full_matched, conf.int = TRUE) %>%
  filter(term == "treatment:opening") %>%
  mutate(model = "Модель 4")
opening_mathcing_df <- tidy(model_opening_matched, conf.int = TRUE) %>%
  filter(term == "treatment:opening") %>%
  mutate(model = "Модель 5")
full_caliper_df <- tidy(model_full_matched_caliper, conf.int = TRUE) %>%
  filter(term == "treatment:opening") %>%
  mutate(model = "Модель 7")
opening_caliper_df <- tidy(model_opening_matched_caliper, conf.int = TRUE) %>%
  filter(term == "treatment:opening") %>%
  mutate(model = "Модель 8")

final_df <- bind_rows(opening_caliper_df, full_caliper_df, opening_mathcing_df, full_matching_df, opening_df, full_df)

final_df <- final_df %>%
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )

library(ggplot2)
library(grid)   

p <- ggplot(final_df, aes(x = model, y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, color = "darkblue") +
  geom_text(aes(label = paste0(round(estimate, 2), "%")), 
            vjust = -1, size = 6) +
  geom_text(aes(y = conf.high, label = paste0(round(conf.high, 2), "%")), 
            hjust = -0.2, vjust = 0.5, size = 5, color = 'black') +
  geom_text(aes(y = conf.low,  label = paste0(round(conf.low,  2), "%")), 
            hjust =  1.2, vjust = 0.5, size = 5, color = 'black') +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  theme_light(base_size = 16) +      
  theme(
    axis.text.x  = element_text(size = 16, color = 'black'),  
    axis.text.y  = element_text(size = 16, color = 'black'),
    axis.title   = element_text(size = 16),
    plot.margin  = margin(8, 20, 10, 20)    
  ) +
  labs(x = NULL, y = NULL)

print(p)

ggsave("Дов_интервалы_открытие.png", plot = p,
       width  = 10,   
       height = 6,    
       dpi    = 300)
```

``` r
full_df <- tidy(model_full, conf.int = TRUE) %>%
  filter(term == "treatment:announcement") %>%
  mutate(model = "Модель 1")
opening_df <- tidy(model_announcement, conf.int = TRUE) %>%
  filter(term == "treatment:announcement") %>%
  mutate(model = "Модель 3")
#full_matching_df <- tidy(model_full_matched, conf.int = TRUE) %>%
#  filter(term == "treatment:announcement") %>%
#  mutate(model = "Модель 4")
opening_mathcing_df <- tidy(model_announcement_matched, conf.int = TRUE) %>%
  filter(term == "treatment:announcement") %>%
  mutate(model = "Модель 6")
full_caliper_df <- tidy(model_full_matched_caliper, conf.int = TRUE) %>%
  filter(term == "treatment:announcement") %>%
  mutate(model = "Модель 7")
opening_caliper_df <- tidy(model_announcement_matched_caliper, conf.int = TRUE) %>%
  filter(term == "treatment:announcement") %>%
  mutate(model = "Модель 9")

final_df <- bind_rows(opening_caliper_df, full_caliper_df, opening_mathcing_df, opening_df, full_df)

final_df <- final_df %>%
  mutate(
    estimate = estimate * 100,
    conf.low = conf.low * 100,
    conf.high = conf.high * 100
  )


p <- ggplot(final_df, aes(x = model, y = estimate)) +
  geom_point(size = 3, color = "darkorange") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, color = "darkorange") +
  geom_text(aes(label = paste0(round(estimate, 2), "%")), 
            vjust = -1, size = 6) +
  geom_text(aes(y = conf.high, label = paste0(round(conf.high, 2), "%")), 
            hjust = -0.2, vjust = 0.5, size = 5, color = 'black') +
  geom_text(aes(y = conf.low,  label = paste0(round(conf.low,  2), "%")), 
            hjust =  1.2, vjust = 0.5, size = 5, color ='black') +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
  theme_light(base_size = 16) +      
  theme(
    axis.text.x  = element_text(size = 14, color = 'black'),  
    axis.text.y  = element_text(size = 14, color = 'black'),
    axis.title   = element_text(size = 16),
    plot.margin  = margin(10, 20, 10, 20)    
  ) +
  labs(x = NULL, y = NULL)

print(p)

ggsave("Дов_интервалы_объявление.png.png", plot = p,
       width  = 10,   
       height = 6,    
       dpi    = 300)
```
