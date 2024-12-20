# Заполнение Пропущенных Значений

Если в датафрейме есть множество пустых значений, которые невозможно заполнить интерполяцией, но они по логике должны быть согласованы с другими данными, вы можете использовать несколько подходов:

---

### **1. Заполнение средним или медианным значением**
Если пропущенные значения не должны сильно выбиваться, их можно заполнить:
- Средним значением (подходит для нормально распределённых данных).
- Медианным значением (лучше для данных с выбросами).

```R
df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
```

**Альтернатива с медианой:**
```R
df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
```

---

### **2. К ближайшему известному значению**
Заполнение пропусков предыдущим или следующим известным значением. Этот метод подходит для временных рядов.

```R
library(zoo)

df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), ~ na.locf(., na.rm = FALSE))) %>% # Пропуски предыдущим
  mutate(across(where(is.numeric) & !contains("Year"), ~ na.locf(., fromLast = TRUE, na.rm = FALSE))) # Пропуски следующим
```

---

### **3. Использование регрессионных моделей**
Для заполнения пропущенных значений можно построить регрессионные модели, используя известные данные. Например, если пропущенное значение одной переменной можно объяснить значениями других переменных:

```R
library(mice)

# Импутация пропусков с использованием модели
imputed_data <- mice(df, method = "pmm", m = 5, maxit = 10, seed = 123)

# Получение итогового датафрейма
df_filled <- complete(imputed_data)
```

**Примечание:**
- `method = "pmm"` — метод предсказания среднего значения с использованием регрессии.
- `m = 5` — количество итераций.

---

### **4. Прогнозирование значений**
Если данные имеют временную структуру или их можно описать временными паттернами, пропущенные значения можно заполнить с использованием моделей, таких как ETS или ARIMA.

Пример:
```R
library(forecast)

df_filled <- df
for (col in colnames(df)[-1]) { # Пропускаем колонку Year
  ts_data <- ts(df[[col]], start = min(df$Year), frequency = 1)
  filled <- na.interp(ts_data) # Автоматическая интерполяция с помощью ETS
  df_filled[[col]] <- as.numeric(filled)
}
```

---

### **5. Заполнение ближайшими соседями (kNN)**
Метод k ближайших соседей (kNN) использует значения других наблюдений с похожими характеристиками для заполнения пропусков.

```R
library(VIM)

df_filled <- kNN(df, k = 5)
```

**Параметры:**
- `k = 5` — количество ближайших соседей.
- Метод подходит для смешанных данных (числовые и категориальные).

---

### **6. Комбинация подходов**
Вы можете комбинировать подходы, чтобы улучшить качество заполнения:
1. Сначала заполнить легко предсказуемые значения (например, с помощью интерполяции).
2. Оставшиеся пропуски заполнить медианой или регрессией.

```R
df_filled <- df %>%
  mutate(across(where(is.numeric), ~ na.approx(., na.rm = FALSE))) %>%  # Интерполяция
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) # Среднее для остальных
```

---

### Какой подход выбрать?

- **Интерполяция**: если данные имеют временную зависимость.
- **Среднее/медиана**: если значения не зависят от порядка.
- **Регрессия**: если пропущенные значения можно объяснить с помощью других переменных.
- **kNN или прогнозирование**: если данные сложные и имеют скрытые зависимости.

Если нужно помочь с вашим конкретным набором данных, напишите!