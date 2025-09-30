# 📊 Dashboard Presupuesto GCBA

Este proyecto es un **dashboard interactivo en R Shiny** que analiza la evolución del presupuesto del Gobierno de la Ciudad de Buenos Aires (GCBA), ajustado por inflación, para el período **2022–2024**.

---

## 📌 Objetivos del análisis

El dashboard busca responder preguntas clave:

- ✅ ¿Cuáles fueron los programas con menos financiación en 2022–2024?  
- ✅ ¿Cuáles fueron los de mayor financiación en 2022–2024?  
- ✅ ¿Se modificaron competencias en la Jefatura de Gobierno?  
- ✅ ¿Qué tendencias se observan en el período analizado?  

Se seleccionaron **5 áreas de interés**:
- Ministerio de Educación  
- Ministerio de Salud  
- Ministerio de Espacio Público  
- Jefatura de Gobierno  
- Ministerio de Desarrollo Humano  

---

## 📂 Estructura del proyecto
DashboardPresupuesto/
├── app.R # Código de la Aplicación del dashboard
├── presupuesto.csv # Presupuesto nominal por programa
├── presupuesto_inflacion.csv # Multiplicador para calcular presupuesto real
└── README.md

📈 Visualizaciones incluidas

<img width="986" height="784" alt="image" src="https://github.com/user-attachments/assets/1bc3eb8c-9702-4357-997b-4f40a1462524" />

<img width="1338" height="784" alt="image" src="https://github.com/user-attachments/assets/c804c482-22db-4518-aa13-c2f1a9f4f8db" />

<img width="1678" height="841" alt="image" src="https://github.com/user-attachments/assets/fab2f3df-8d42-4ce8-ad4a-a4fb2895344d" />


- Inicio: preguntas de investigación.

- Presupuesto (por año): tarjetas clickeables por área → redirigen a detalle.

- Detalle de área: Top 5 y Bottom 5 programas del año seleccionado.

- Evolución: Presupuesto nominal vs real.

- Análisis 2022–2024: Top 3 programas con mayor crecimiento y con mayor caída por área.

- Análisis de KPI encontrados

-Políticas Públicas Basadas en Datos


🛠️ Tecnologías usadas: R, Shiny, Shinydashboard, Tidyverse, PowerQuery, PowerBI. 

✍️ Autora: Giselle San German
📅 Año: 2025
