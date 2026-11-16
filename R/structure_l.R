
st_market_phi <- 1.1877556

st_with_market_phi <- function(p = st_defaults(), phi = st_market_phi) { p$ppw <- p$ppw * phi; p$floor <- p$floor * phi; p }
