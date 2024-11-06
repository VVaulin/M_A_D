#Дисперсионный анализ. Лабораторная
#Ваулин В.И.
#РИ-411055

#install.packages("gplots")
#install.packages("multcomp")

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
data = read.csv("C:/Users/vladv/Downloads/lecture 5.1/data/diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#files/Diet_data_description.docx

colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
summary(data) 

# разделим данные на три группы по номеру диеты:

data.A <- subset(data, diet.type == "A")
data.B <- subset(data, diet.type == "B")
data.C <- subset(data, diet.type == "C")

# выбросы по каждой группе:
out.A <- boxplot.stats(data.A$weight.loss)$out 
out.B <- boxplot.stats(data.B$weight.loss)$out
out.C <- boxplot.stats(data.C$weight.loss)$out

out.A
out.B
out.C

boxplot(data.A$weight.loss) # на диаграмме видны выбросы в 1 группе
boxplot(data.B$weight.loss) # нет выбросов
boxplot(data.C$weight.loss) # нет выбросов

# выбросы есть только в 1 группе, удалим их:
fixeddata <- subset(data, weight.loss != 8.5 & weight.loss != 9.0)
fixeddata.A <- subset(fixeddata, diet.type == "A")
boxplot.stats(fixeddata.A$weight.loss)$out# теперь выбросов нет
boxplot(fixeddata.A$weight.loss)
boxplot(fixeddata.A$weight.loss) #диаграмма 1 группы без выбросов


# Проанализиуем различия по типам диет

#с выбросами:
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

# без выбросов:
boxplot(weight.loss~diet.type,data=fixeddata,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")
# без выбросов потеря веса при 1 диете меньше, чем с выбросами

#проверим сбалансированные ли данные
table(data$diet.type)
table(fixeddata$diet.type)


# График групповых средних
# с выбросами:
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

# без выбросов:
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=fixeddata)
aggregate(fixeddata$weight.loss, by = list(fixeddata$diet.type), FUN=sd)
# без выбросов среднеквадратичное отклонение меньше, на графике  
# групповых средних это также видно

# Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
# с выбросами:
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

# без выбросов:
fixed.fit <- aov(weight.loss ~ diet.type, data=fixeddata)
summary(fixed.fit)

# попарные различия между средними значениями для всех групп
# с выбросами:
TukeyHSD(fit)

# без выбросов:
TukeyHSD(fixed.fit)


# Tukey honest significant differences test
# с выбросами:
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

# без выбросов:
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fixed.fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
