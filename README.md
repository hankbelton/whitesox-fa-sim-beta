# White Sox free agent simulator (beta)

I built this as a front-office decision tool for the White Sox' 2026-27 offseason. My model supplies the distributions: each free agent's
market price, his projected WAR paths for 2027-2031, and what his wins are worth to the Sox at their own win level. You supply the
conviction, the risk appetite and the priorities, then log moves one at a time; every move re-values the Sox roster and the rest of the Board.

**Beta. Numbers may change until the final release.**

## What is in this repository

- `app.R` and `R/`: the app (R Shiny).
- `data/`: derived numbers only: my model's prices and ranges, my engine's WAR paths, the Sox win path, win-value curves, payroll and roster
  facts, and backtest scores of my models. No raw third-party tables are included, and no player statistics from other sites are shown.

## Run it

```r
install.packages(c("shiny", "bslib", "data.table", "DT", "ggplot2", "yaml"))
shiny::runApp(".")
```

## Sources and credits

I'm grateful to the people and sites whose public work this rests on:

- **Projections:** Steamer 2027 (FanGraphs) is the input to my WAR path engine. Only my engine's derived values are shown.
- **Market prices:** my own price model for every player.
- **Signings:** as reported by MLB Trade Rumors and the MLB transactions feed. For players who have signed, the market ticker shows MLB Trade
  Rumors' predicted AAV, credited.
- **Payroll and roster:** MLB Stats API (rosters, options); contracts from public reports, with Cot's Baseball Contracts credited where it
  settled a disputed term; league payrolls from AP opening-day lists; the spending limit's "dial it up" figure from Spotrac team payrolls.
- **Win value:** attendance (Baseball-Reference), revenue per fan (Atlanta Braves Holding Company 10-K, Forbes team pages), postseason gate
  (MLB).
- **Draft-pick values:** Edwards (FanGraphs, 2019).

The app is an independent project. It is not affiliated with or endorsed by the Chicago White Sox, Major League Baseball or any source above.

Model and app by Henry.
