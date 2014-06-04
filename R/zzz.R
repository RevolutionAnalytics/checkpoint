rtt_compact <- function (l) Filter(Negate(is.null), l)

mssg <- function(x, ...) if(x) message(...)
