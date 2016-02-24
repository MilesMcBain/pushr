#Test Code
a <- trees
b <- data.frame(list(v1 = 1.0,v2 = 1, v3 = 1.0))
c <- data.frame(list(v1 = 2.0,v2 = 2, v3 = 2.0))
push(b,a)
a
push(b,a)
a
push_initialise(a)
push(c,a)
