# backtest
R Package to Backtest Time Series Models

    library(devtools)
    install_github("joshoberman/backtest")

This package will run backtests on VAR, ARIMA, ETS, and MARS models for time series forecasting. It's main goal is to provide a unified interface for predictor selection in VAR models, and to afford a direct comparison between VAR and other time series models available in R.

The package exports a single function, runCalc(), that expects a well-formed JSON, an example of which can be found in the package directory after installation.

For more direct inquiries please email joshoberman@gmail.com
