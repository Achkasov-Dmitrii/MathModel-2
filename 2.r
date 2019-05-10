#создайте модель множественной линейной регрессии 
#дневных потоков паров воды за летний период 2013 года по данным измерений методом турбулентной пульсации
# Для выбора нужных суток используйте переменную DOY - день года (1 января - DOY = 1)

setwd ("I:\\model") 
getwd() 
#подключаем tidyverse 
library(tidyverse) 
library(lubridate) 
#data=write.csv("eddypro.csv") 
#читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
data = read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
#ещё одну строку - долой! 
data = data[-1,] 
#убираем ненужные колонки 
data = data[, c(-1, -3, -9, -12, -15, -18, -21, -30, -35, -63 , -70, -88:-99) ] 
#преобразуем строковые значения в факторные 
data = data %>% mutate_if(is.character, factor) 
#заменяем конфликтующие знаки колонок 
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
#Посмотрим, что получилось 
glimpse(data) 
#Если бы нам требовалось выбрать ночное или дневное время, мы бы изменили тип данных колонки daytime и использовали её в следующей команде 
#data$daytime = as.logical(data$daytime) 
#оставим данные только по весеннему периоду 2013 года: 
data = data[data$DOY >= 152 & data$DOY <= 243 & year(data$date) == 2013, c(1:ncol(data))] 
#выберем все переменные типа numeric 
data_numeric = data[,sapply(data,is.numeric) ] 
#все остальные переменные: 
data_non_numeric = data[,!sapply(data,is.numeric) ] 
# создадим матрицу для корелляционного анализа и преобразуем ее в таблицу, выбрав нужный столбец (потоки паров воды) 
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(co2_flux) 
#выберем имена переменных (строк) с коэффициентом детерминации больше 0.2 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .2] %>% na.exclude; vars 
#соберем переменные из вектора в одну формулу: 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")); formula 
#создадим обучающую и тестирующую выборки: 
row_numbers = 1:length(data$date) 
teach = sample(row_numbers, floor(length(data$date)*.7)) 
test = row_numbers[-teach] 
#непересекающиеся подвыборки: 
teaching_tbl_unq = data[teach,] 
testing_tbl_unq = data[test,] 

# МОДЕЛЬ 1 
#создаем модель линейной регрессии 
model = lm(formula, data = data);model 
#коэффициенты 
coef(model) 
#остатки 
resid(model) 
#доверительный интервал 
confint(model) 
#P-значения по модели 
summary(model) 
#дисперсионный анализ 
anova(model) 
#графическое представление модели: 
plot(model) 

#Модель 2 
formula2 = co2_flux~Tau + rand_err_Tau + H + 
  LE + rand_err_LE + h2o_flux + rand_err_h2o_flux + 
  co2_molar_density + co2_mixing_ratio + 
  sonic_temperature + air_temperature + 
  u. + T. +un_H + un_LE + LE_scf + un_co2_flux + un_h2o_flux 

#создаем модель линейной регрессии 
model2 = lm(formula2, data = data);model 
#коэффициенты 
coef(model2) 
#остатки 
resid(model2) 
#доверительный интервал 
confint(model2) 
#P-значения по модели 
summary(model2) 
#дисперсионный анализ 
anova(model2) 
#графическое представление модели: 
#plot(model1) 

# МОДЕЛЬ 3 
formula3 = co2_flux~Tau + rand_err_Tau + H + 
  LE + rand_err_LE + h2o_flux + rand_err_h2o_flux + 
  co2_molar_density + co2_mixing_ratio + 
  sonic_temperature + air_temperature + 
  u. + T. +un_H + un_LE + LE_scf 
#создаем модель линейной регрессии 
model3 = lm(formula3, data = data);model 
#коэффициенты 
coef(model3) 
#остатки 
resid(model3) 
#доверительный интервал 
confint(model3) 
#P-значения по модели 
summary(model3) 
#дисперсионный анализ 
anova(model3) 
#графическое представление модели: 
plot(model3) 




#модель4 
formula4 = co2_flux~Tau + rand_err_Tau + H + 
  LE + rand_err_LE + h2o_flux + 
  co2_molar_density + co2_mixing_ratio + 
  sonic_temperature + T. 
#создаем модель линейной регрессии 
model4 = lm(formula4, data = data);model

#коэффициенты 
coef(model4) 
#остатки 
resid(model4) 
#доверительный интервал 
confint(model4) 
#P-значения по модели 
summary(model4) 
#дисперсионный анализ 
anova(model4) 
#графическое представление модели: 
plot(model4) 


formula5 = co2_flux~Tau + rand_err_Tau + H + 
  LE + rand_err_LE + h2o_flux + 
  co2_molar_density + co2_mixing_ratio + 
  sonic_temperature + T. + (Tau + rand_err_Tau + H + 
                              LE + rand_err_LE + h2o_flux + 
                              co2_molar_density + co2_mixing_ratio + 
                              sonic_temperature + T.)^2;formula5 
#создаем модель линейной регрессии 
model5 = lm(formula5, data = data); model 
#коэффициенты 
coef(model5) 
#остатки 
resid(model5) 
#доверительный интервал 
confint(model5) 
#P-значения по модели 
summary(model5) 
#дисперсионный анализ 
anova(model5) 
#графическое представление модели: 
plot(model5)

