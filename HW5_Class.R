## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

# You must create a validity method with setValidity() that determines whether 
# an object is a valid sparse_numeric object.

setValidity ("sparse_numeric", function (object) {
    v  <- object@value
    p  <- object@pos
    n  <- object@length
    
    # test case: "check validity method 2"
    if (length (p) > n)
      return ("The number of sparse elements must be fewer than the length of
              the vector.")
    
    # Not covered in the test cases:
    if (any (p < 1L) || any (p > n))
      return ("Every index position must be in [1, length]")

    if (length (v) != length (p))
        return ("The length of values must be the same as that of indices.")

    if (!is.numeric (v) || any (!is.finite (v)))
        return ("Each value must be a finite numeric.")

    if (!is.integer (p) || any (duplicated (p)))
        return ("Each index must be a unique integer.")
    
    if (!is.integer (n))
        return ("The length of the vector must be an integer")

    TRUE
})

# You must create three generic functions and their corresponding methods:
# For each of the four generic functions above, you should implement a method 
# that dispatches on two sparse_numeric objects and returns the appropriate 
# value. 
# For each of the arithmetic functions you should check to see that the 
# arguments are of the same length. If they are not the same length your 
# function should return an error via stop().


# 1. sparse_add(x, y, ...): add two sparse_numeric vectors and return a 
# sparse_numeric object
setGeneric ("sparse_add", function (x, y, ...) standardGeneric ("sparse_add"))
setMethod ("sparse_add",
           signature (x = "sparse_numeric", y = "sparse_numeric"),
           function (x, y) {
             if (x@length != y@length)
               stop ("Sparse vectors must have the same length.")
             
             # Initialize the output vector
             # It could be longer than both x and y
             output_value <- numeric(0L)
             output_pos <- integer(0L)
             
             # Save fields in local variables to minimize access time
             x_value <- x@value
             x_pos <- x@pos
             y_value <- y@value
             y_pos <- y@pos
             
             # Initialize 2 pointers in the pos vectors for x and y to iterate
             i <- 1L
             j <- 1L
             
             # Set the limit of the 2 pointers
             x_pos_n <- length (x_pos)
             y_pos_n <- length (y_pos)
             
             # Iterate through the sparce vectors
             while (i <= x_pos_n && j <= y_pos_n) {
               # If we are at the same index, sum x[i] and y[j]
               if (x_pos[i] == y_pos[j]) {
                 s <- x_value[i] + y_value[j]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, x_pos[i])
                 }
                 # Move the pointers to point to the next pos 
                 i <- i + 1L
                 j <- j + 1L
               } 
               # If x has a sparse element at an earlier index
               else if (x_pos[i] < y_pos[j]) {
                 s <- x_value[i]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, x_pos[i])
                 }
                 # Only move x pointer to the next pos
                 i <- i + 1L
               } 
               # If y has a sparse element at an earlier index
               else {
                 s <- y_value[j]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, y_pos[j])
                 }
                 # Only move y pointer to the next pos
                 j <- j + 1L
               }
             }
             
             # If x has more sparse elements
             while (i <= x_pos_n) {
               s <- x_value[i]
               if (s != 0) {
                 output_value <- c (output_value, s)
                 output_pos <- c (output_pos, x_pos[i])
               }
               i <- i + 1L
             }
             
             # If y has more sparse elements
             while (j <= y_pos_n) {
               s <- y_value[j]
               if (s != 0) {
                 output_value <- c (output_value, s)
                 output_pos <- c (output_pos, y_pos[j])
               }
               j <- j + 1L
             }
             
             # Return the new sparse vector
             new ("sparse_numeric",
                  value  = output_value,
                  pos    = as.integer (output_pos),
                  length = x@length)
           })

# 2. sparse_mult(x, y, ...): multiply two sparse_numeric vectors and return a 
# sparse_numeric object

setGeneric ("sparse_sub", function (x, y, ...) standardGeneric ("sparse_sub"))
setMethod ("sparse_sub",
           signature (x = "sparse_numeric", y = "sparse_numeric"),
           function (x, y) {
             if (x@length != y@length)
               stop ("Sparse vectors must have the same length.")
             
             # Initialize the output vector
             # It could be longer than both x and y
             output_value <- numeric(0L)
             output_pos <- integer(0L)
             
             # Save fields in local variables to minimize access time
             x_value <- x@value
             x_pos <- x@pos
             y_value <- y@value
             y_pos <- y@pos
             
             # Initialize 2 pointers in the pos vectors for x and y to iterate
             i <- 1L
             j <- 1L
             
             # Set the limit of the 2 pointers
             x_pos_n <- length (x_pos)
             y_pos_n <- length (y_pos)
             
             # Iterate through the sparce vectors
             while (i <= x_pos_n && j <= y_pos_n) {
               # If we are at the same index, x[i] - y[j]
               if (x_pos[i] == y_pos[j]) {
                 s <- x_value[i] - y_value[j]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, x_pos[i])
                 }
                 # Move the pointers to point to the next pos 
                 i <- i + 1L
                 j <- j + 1L
               } 
               # If x has a sparse element at an earlier index
               else if (x_pos[i] < y_pos[j]) {
                 s <- x_value[i]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, x_pos[i])
                 }
                 # Only move x pointer to the next pos
                 i <- i + 1L
               } 
               # If y has a sparse element at an earlier index
               else {
                 s <- 0 - y_value[j]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, y_pos[j])
                 }
                 # Only move y pointer to the next pos
                 j <- j + 1L
               }
             }
             
             # If x has more sparse elements
             while (i <= x_pos_n) {
               s <- x_value[i]
               if (s != 0) {
                 output_value <- c (output_value, s)
                 output_pos <- c (output_pos, x_pos[i])
               }
               i <- i + 1L
             }
             
             # If y has more sparse elements
             while (j <= y_pos_n) {
               s <- 0 - y_value[j]
               if (s != 0) {
                 output_value <- c (output_value, s)
                 output_pos <- c (output_pos, y_pos[j])
               }
               j <- j + 1L
             }
             
             # Return the new sparse vector
             new ("sparse_numeric",
                  value  = output_value,
                  pos    = as.integer (output_pos),
                  length = x@length)
           })

# 3. sparse_sub(x, y, ...): subtract two sparse_numeric vectors and return a 
# sparse_numeric object

setGeneric ("sparse_mult", function (x, y, ...) standardGeneric ("sparse_mult"))
setMethod ("sparse_mult",
           signature(x = "sparse_numeric", y = "sparse_numeric"),
           function(x, y) {
             if (x@length != y@length)
               stop ("Sparse vectors must have the same length.")
            
             # Initialize the output vector
             # It could be longer than both x and y
             output_value <- numeric(0L)
             output_pos <- integer(0L)
             
             # Save fields in local variables to minimize access time
             x_value <- x@value
             x_pos <- x@pos
             y_value <- y@value
             y_pos <- y@pos
             
             # Initialize 2 pointers in the pos vectors for x and y to iterate
             i <- 1L
             j <- 1L
             
             
             while (i <= length (x_pos) && j <= length (y_pos)) {
               # Multiplication only works when both values exist at one index
               if (x_pos[i] == y_pos[j]) {
                 s <- x_value[i] * y_value[j]
                 if (s != 0) {
                   output_value <- c (output_value, s)
                   output_pos <- c (output_pos, x_pos[i])
                 }
                 # Move both pointers
                 i <- i + 1L
                 j <- j + 1L
               } 
               else if (x_pos[i]< y_pos[j]) {
                 # Move x pointer
                 i <- i + 1L
               } 
               else {
                 # Move y pointer
                 j <- j + 1L
               }
             }
             
             # Return the new sparse vector
             new ("sparse_numeric",
                  value  = output_value,
                  pos    = as.integer (output_pos),
                  length = x@length)
           })

# 4. sparse_crossprod(x, y, ...): take the cross product of two sparse_numeric 
# vectors and return a numeric vector of length 1.

setGeneric ("sparse_crossprod", function (x, y, ...) 
            standardGeneric ("sparse_crossprod"))
setMethod ("sparse_crossprod",
           signature(x = "sparse_numeric", y = "sparse_numeric"),
           function(x, y) {
             if (x@length != y@length)
               stop("Sparse vectors must have the same length.")
             
             # Save fields in local variables to minimize access time
             x_value <- x@value
             x_pos <- x@pos
             y_value <- y@value
             y_pos <- y@pos
             
             # Initialize 2 pointers in the pos vectors for x and y to iterate
             i <- 1L
             j <- 1L
             
             # Initialize the cross product to 0
             s <- 0
             
             while (i <= length (x_pos) && j <= length (y_pos)) {
               # Cross product is the sum of products at each index
               if (x_pos[i] == y_pos[j]) {
                 s <- s + x_value[i] * y_value[j]
                 i <- i + 1L
                 j <- j + 1L
               } 
               else if (px[i] < py[j]) {
                 i <- i + 1L
               } 
               else {
                 j <- j + 1L
               }
             }
             
             # Return cross product
             as.numeric(s)
           })

# In addition, you should also implement methods for +, *, and -, which 
# correspond to sparse_add(), sparse_mult(), and sparse_sub().


setMethod ("+", c ("sparse_numeric", "sparse_numeric"),
          function (e1, e2) sparse_add (e1, e2))

setMethod ("-", c ("sparse_numeric", "sparse_numeric"),
          function (e1, e2) sparse_sub (e1, e2))

setMethod ("*", c ("sparse_numeric", "sparse_numeric"),
          function (e1, e2) sparse_mult (e1, e2))

# You should implement two coercion methods using setAs():

# One to coerce from numeric to sparse_numeric
# Must use from as the argument name
setAs ("numeric", "sparse_numeric",
       function (from) {
         # Find indices of non-zero sparse elements in the regular vector
         indices <- which (from != 0)
         new("sparse_numeric",
             value = from[indices],
             pos   = as.integer(indices),
             length = as.integer(length(from)))
       })

# One to coerce from sparse_numeric to numeric
setAs("sparse_numeric", "numeric",
      function(from) {
        regular_vector <- numeric(from@length)
        regular_vector[from@pos] <- from@value
        regular_vector
      })



# You should implement a show() method that prints sparse_numeric vectors to the
# console in some way.
setMethod ("show", "sparse_numeric", function(object) {
  cat ("An object of class 'sparse_numeric'\n")
  cat (" Length:", object@length, "\n", sep = " ")
  if (length(object@pos) == 0L) {
    cat (" All elements are zero.\n")
  } 
  else {
    cat (" Non-zero positions:", object@pos, "\n")
    cat (" Corresponding values:", object@value, "\n")
  }
})

# You should implement a plot() method that plots the overlapping non-zero 
# elements of two sparse_numeric vectors.
setMethod ("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  common <- intersect (x@pos, y@pos)
  
  x_value <- x@value[match(common, x@pos)]
  y_value <- y@value[match(common, y@pos)]
  
  plot (common, x_value,
        xlab = "Position",
        ylab = "Values",
        main = "Overlapping Non-Zero Elements of 2 Sparse Vectors",
        pch = 19)
  
  points (common, y_value, pch = 17)
})
            

# Write a new generic function and then implement the corresponding method for 
# sparse_numeric objects
setGeneric ("count_sparse", function (x, ...) standardGeneric ("count_sparse"))

setMethod ("count_sparse", "sparse_numeric", function(x, ...) {
  length (x@pos)
})

