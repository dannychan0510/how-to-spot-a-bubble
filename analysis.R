y <- as.matrix(y)
N <- nrow(y)
lagy <- as.matrix(numeric(N))
lagy[2:N, 1] <- y[1:N-1, 1]
dy <- y - lagy
dy <- as.matrix(dy[2:N, 1])
N <- nrow(dy)
mfit <- 10

statvector <- as.matrix(numeric(N))

i <- mfit

while(i <= N){
  dysub <- as.matrix(dy[(i - mfit + 1):i, 1])
  num <- sum(cumsum(rev(dysub)))
  denom <- sqrt(t(dysub) %*% dysub)
  statvector[i, 1] <- num / denom
  i <- i + 1
}

critvector <- statvector[mfit:(N - mfit), 1]
cv <- quantile(critvector, 0.95)
teststat <- statvector[N, 1]

ifelse(teststat > cv, print("Bubble"), print("No Bubble"))


# Backwards recursive test of Philips & co (good for detecting end of bubbles, whilst above is good for detecting beginning of bubble)
z <- read.table("C:/Users/895284/Desktop/Bubble/dB.txt", quote="\"")
z <- as.matrix(z)
N = nrow(z)
cv <- 1.7 # Takes hours to calcualate individual crit values, so preferable to assume 1.7, as it doesn't vary much anyways
min_DF_length <- floor((0.01 + 1.8 / sqrt(N)) * N)
ADF_start <- as.matrix(numeric(N-min_DF_length + 1))