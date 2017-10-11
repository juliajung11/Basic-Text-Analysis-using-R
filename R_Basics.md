# R Basics
Justin Ho  
10/11/2017  



## R Basics

R is simply a calculator, you can perform simple computation by typing the equation.

```r
3+4
```

```
## [1] 7
```

You can define a new object with the assignment operator "<-".

```r
result <- 3+4
```

When defining a object, nothing will happen when sucessful (no news is good news). To view the object, you can simply type the name of the object.

```r
result
```

```
## [1] 7
```

You can also put string inside an object, remember the quotation mark.

```r
text <- "This is a test message"
```

To view it, type the name of the object.

```r
text
```

```
## [1] "This is a test message"
```

A vector is a sequence of data elements, we can use "c()" to define a vector.

```r
vector <- c(1, 2, 5, 7, 8, 9, 10)
vector
```

```
## [1]  1  2  5  7  8  9 10
```

You can access a particular compenent of a vector (or many other object types) by using the slicing operator "[]".

```r
vector[1] # It shows the first element.
```

```
## [1] 1
```

```r
vector[3] # It shows the third element.
```

```
## [1] 5
```

```r
vector[1:3] # It shows elements from the first to the second one.
```

```
## [1] 1 2 5
```

```r
vector[-1] # It shows all the elements EXCEPT the first one.
```

```
## [1]  2  5  7  8  9 10
```

A function is a group of prewritten codes, it takes in data and produces output. For example, "summary()" is a often used function.

```r
summary(vector)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0     3.5     7.0     6.0     8.5    10.0
```

If you are calling a gigatic object, you don't want to mess up your screen, you might use "head()" to shows the first few components/lines.

```r
head(vector)
```

```
## [1] 1 2 5 7 8 9
```

To know what a function does, use "?" .

```r
?head
```

Most functions take in extra arguments too.

```r
head(vector, 2) # showing the first two components
```

```
## [1] 1 2
```

It works for vector of strings too.

```r
textvector <- c("This", "is", "a", "test", "message", ".")
textvector
```

```
## [1] "This"    "is"      "a"       "test"    "message" "."
```

```r
textvector[2]
```

```
## [1] "is"
```

```r
head(textvector, 3)
```

```
## [1] "This" "is"   "a"
```

```r
summary(textvector)
```

```
##    Length     Class      Mode 
##         6 character character
```

You can nest the function like this.

```r
summary(head(vector))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.750   6.000   5.333   7.750   9.000
```

Alternatively, if you load the magrittr package, you can pipe it. It does the same thing, but is written in a slightly amiable way.

```r
library(magrittr)
head(vector) %>% summary()
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   2.750   6.000   5.333   7.750   9.000
```

