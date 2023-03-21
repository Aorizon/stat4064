#Base
lines = 10
workers = 20
units_pre <- 1.2+3.4*lines+0.22*workers+1.4*(lines*workers)

#Add a line
lines = lines + 1
units_post <- 1.2+3.4*lines+0.22*workers+1.4*(lines*workers)
units_extra = units_post - units_pre
units=c(units_pre,units_post,units_post/units_pre)
print(units)