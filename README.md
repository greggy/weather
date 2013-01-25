## Проект WeatheR - обработка погодной информации средствами R

### Задача 1 (done)

Обеспечить отрисовку графиков температуры за год. Несколько графиков за 
разные года должны выводиться рядом.
На графике по оси Х должны выводиться месяца с какими то еще делениями для 
лучшей ориентации.
Из массива данных необходимо выбирать пораздельности данные по температуре 
днем - они отрисовываются напрмер красным, и отдельно данные по температуре 
ночью - они отрисовываются синим. Это на одном графике - должно быть наглядно 
видна разницам между дневными и ночными температурами. Из-за пропущенных 
данных и необходимости выводить месяца - необходимо из первой колонки 
полностью парсить временную метку.

Скрипт Weathe.R - выводит график по всем температурам. Из исходного файла 
удален заголовок.

#### Usage:
    > source("weathe.R")
    > yplot(2006)
    > yplot(2009:2011)


### Задача 2

Дополнительно к температурным графикам добавлять график по другому параметру -
давление, влажность, осадки?

### Usage:
    > source("weathe.R")
    > yplot(2006, pres = TRUE)
    > yplot(2009:2011, = TRUE)


### Задача 3

Обеспечить подсчет среднесуточной температуры с аппроксимацией недостающих 
данных. Построить график среднесуточной температуры и метки для определения, 
когда она становится выше +10 градусов.
