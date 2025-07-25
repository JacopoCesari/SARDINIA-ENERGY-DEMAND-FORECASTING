# Sardinia Energy Demand Forecasting
This project analyzes electricity demand patterns in Sardinia to identify the best time window for planned power system testing. Using historical data from 2020-2025, we developed forecasting models to predict when energy consumption is lowest, minimizing the impact of temporary blackouts on daily life.

The analysis focuses on a 3-day period (February 28 - March 2, 2025) to find the optimal 1-hour window for conducting stress tests on the regional energy grid without causing major disruptions to citizens and businesses.

---

# Previsione della Domanda Energetica in Sardegna
Questo progetto analizza i pattern di consumo elettrico in Sardegna per identificare la finestra temporale migliore per test pianificati del sistema energetico. Utilizzando dati storici dal 2020-2025, abbiamo sviluppato modelli di previsione per predire quando il consumo energetico è più basso, minimizzando l'impatto di blackout temporanei sulla vita quotidiana.

L'analisi si concentra su un periodo di 3 giorni (28 febbraio - 2 marzo 2025) per trovare la finestra ottimale di 1 ora per condurre stress test sulla rete energetica regionale senza causare gravi disagi a cittadini e aziende.

## 🎯 Objectives
- Find the best time for planned power outages with minimal impact
- Forecast electricity demand 72 hours in advance
- Support energy system stress testing for security purposes

## 📊 Data
- **Source**: Terna (Italian electricity operator) + weather data
- **Period**: 2020-2025
- **Type**: Hourly electricity consumption and temperature in Sardinia
- **Size**: ~45,000 observations

## 🔧 Models Used
1. **Simple Models**: Seasonal patterns and averages
2. **ETS Models**: Exponential smoothing techniques
3. **SARIMAX**: Advanced time series model with weather data
4. **Linear Regression**: Statistical modeling with multiple variables

## 🏆 Results
- **Best Model**: SARIMAX(3,0,1)(1,0,2)[24]
- **Optimal Time Window**: February 28, 2025 at 3:00 AM
- **Accuracy**: High precision confirmed by statistical tests
- **Impact**: Minimal disruption during lowest demand period

## 📈 Key Findings
- Clear daily, weekly, and seasonal patterns in energy consumption
- Weekend consumption significantly lower than weekdays
- Early morning hours (3-5 AM) show consistently lowest demand
- Temperature strongly influences electricity usage

## 👥 Team
- Giovanni Zecchini
- Jacopo Cesari  
- Tommaso Botticelli
