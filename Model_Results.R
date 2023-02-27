#results

y <- c(0.05,0.005, 0.0005, 0.00005)


tokens_lift2 <- c(0.895,0.894, 0.8955, 0.915)
assns_lift2 <- c(0.286, 0.311, 0.592, 0.719)

tokens_lift10 <- c(0.915836392,0.915836392,0.915836392, 0.915967488)
assns_lift10 <- c(0.976754318,0.917535263,0.815477973,0.769899621)

plot(tokens_lift2, y,  col = 'black', xlim = c(0.89,1))

points(tokens_lift10, y, col = 'blue')


plot(assns_lift2, y, xlim = c(0.25,1), col= "blue")
points(assns_lift10,y, col = "red")
