#bridge hw 1

#Write a loop that calculates 12-factorial
i = 1
j=1
for (i in 1:12) {j=j*i}
j
12*11*10*9*8*7*6*5*4*3*2*1
  
  
# Show how to create a numeric vector that contains the sequence from 20 to 50 by 5
v <- c(0:6)
nv <- 20 + (5*v)
nv

# Create the function “factorial” that takes a trio of input numbers a, b, and c and solve the quadratic
#equation. The function should print as output the two solutions

quad<-function(a, b, c) {
  
  pos_root <- (((-b) + sqrt((b*b) - 4*a*c)) / (2*a))
  
  neg_root <- (((-b) - sqrt((b*b) - 4*a*c)) / (2*a))
  
  print(pos_root)
  
  print(neg_root)
  
}

#two real roots
quad(2, 5, -3)

#one real root
quad(-4, 12, -9)

#no real roots
quad(1, -3, 4)
