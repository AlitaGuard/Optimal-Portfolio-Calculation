# Optimal-Portfolio-Calculation
Functioning codes for investment project. Maybe also for future use?

About `get.MP()`:
- Receives the mean vector `e`, covariance matrix `Sig`, and risk-free rate `rf`
- Returns an object recording the market portfolio
- Set `frontier = TRUE` (default) to draw the efficient frontier
- Further set `line = TRUE` (default) to draw the CML
- Set `new = FALSE` to draw on the existing plot
- You can also pass other plotting options

About `get.optP()`:
- Receives the object returned by `get.MP()` and a user-sepcified parameter `A`
- Returns the optimal portfolio
- Tangent indifferent curves will also be drawn

Demo: 
- Call below to draw the frontier and CML
  `mkt.p <- get.MP(my.return, my.cov, 0.02, col = "blue", lty = 3)`
- Then call below to draw the investor's optimal portfolio
  `inv.p <- get.optP(mkt.p, A = 4, col = "red")`

The output plot may look like:
![plot](https://user-images.githubusercontent.com/60502569/174472904-bde8b85d-1eb5-4b31-b242-830c305cc472.png)

Warnings on sequence length may be raised. You can ignore them
