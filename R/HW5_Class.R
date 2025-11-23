#' @importFrom methods new show
#' @importFrom graphics points
NULL

#' sparse_numeric S4 Class
#'
#' Represents a sparse numeric vector using only non-zero elements.
#'
#' @slot value A numeric vector containing non-zero values.
#' @slot pos An integer vector giving the positions of non-zero values.
#' @slot length An integer giving the full vector length.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' Validity Method for sparse_numeric
#'
#' Ensures that a sparse_numeric object satisfies structural constraints,
#' including index bounds, matching lengths, uniqueness, finiteness, and type requirements.
#'
#' @name sparse_numeric-validity
#' @rdname sparse_numeric-validity
#'
#' @param object A sparse_numeric object.
#'
#' @return TRUE if valid; otherwise a character string describing the problem.
#' @keywords internal
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

#' Add Two Sparse Numeric Vectors
#'
#' Performs element-wise addition of two sparse_numeric vectors.
#'
#' @name sparse_add
#' @rdname sparse_add
#'
#' @aliases sparse_add sparse_add,sparse_numeric,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A sparse_numeric object containing the sum.
#' @export
#' @seealso \code{\link{sparse_sub}}, \code{\link{sparse_mult}}
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

#' Subtract Two Sparse Numeric Vectors
#'
#' Performs element-wise subtraction of two sparse_numeric vectors.
#'
#' @name sparse_sub
#' @rdname sparse_sub
#'
#' @aliases sparse_sub sparse_sub,sparse_numeric,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A sparse_numeric object containing the difference.
#' @export
#' @seealso \code{\link{sparse_add}}, \code{\link{sparse_mult}}
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

#' Multiply Two Sparse Numeric Vectors
#'
#' Performs element-wise multiplication of two sparse_numeric vectors.
#'
#' @name sparse_mult
#' @rdname sparse_mult
#'
#' @aliases sparse_mult sparse_mult,sparse_numeric,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A sparse_numeric object containing the element-wise product.
#' @export
#' @seealso \code{\link{sparse_add}}, \code{\link{sparse_sub}}
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

#' Cross Product of Two Sparse Numeric Vectors
#'
#' Computes the scalar inner product (cross-product) of two sparse_numeric vectors.
#'
#' @name sparse_crossprod
#' @rdname sparse_crossprod
#'
#' @aliases sparse_crossprod sparse_crossprod,sparse_numeric,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A numeric scalar.
#' @export
#' @seealso \code{\link{sparse_mult}}
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
               else if (x_pos[i] < y_pos[j]) {
                 i <- i + 1L
               } 
               else {
                 j <- j + 1L
               }
             }
             
             # Return cross product
             as.numeric(s)
           })

#' Arithmetic Operator Methods for sparse_numeric
#'
#' Provides S4 methods for the arithmetic operators \code{+}, \code{-}, and \code{*}
#' that dispatch to sparse_add(), sparse_sub(), and sparse_mult().
#'
#' @name sparse_numeric-arithmetic
#' @rdname sparse_numeric-arithmetic
#'
#' @aliases
#'   +,sparse_numeric,sparse_numeric-method
#'   -,sparse_numeric,sparse_numeric-method
#'   *,sparse_numeric,sparse_numeric-method
#'
#' @param e1 A sparse_numeric object.
#' @param e2 A sparse_numeric object.
#'
#' @return A sparse_numeric object.
#' @export
setMethod ("+", c ("sparse_numeric", "sparse_numeric"),
           function (e1, e2) sparse_add (e1, e2))

setMethod ("-", c ("sparse_numeric", "sparse_numeric"),
           function (e1, e2) sparse_sub (e1, e2))

setMethod ("*", c ("sparse_numeric", "sparse_numeric"),
           function (e1, e2) sparse_mult (e1, e2))

#' Coerce Numeric Vector to sparse_numeric
#'
#' Converts a numeric vector into a sparse_numeric representation.
#'
#' @name coerce-numeric-sparse
#' @rdname coerce-numeric-sparse
#'
#' @param from A numeric vector.
#' @return A sparse_numeric object.
#' @keywords internal
setAs ("numeric", "sparse_numeric",
       function (from) {
         # Find indices of non-zero sparse elements in the regular vector
         indices <- which (from != 0)
         new("sparse_numeric",
             value = from[indices],
             pos   = as.integer(indices),
             length = as.integer(length(from)))
       })

#' Coerce sparse_numeric to Numeric Vector
#'
#' Converts a sparse_numeric object into a full numeric vector.
#'
#' @name coerce-sparse-numeric
#' @rdname coerce-sparse-numeric
#'
#' @param from A sparse_numeric object.
#' @return A numeric vector.
#' @keywords internal
setAs("sparse_numeric", "numeric",
      function(from) {
        regular_vector <- numeric(from@length)
        regular_vector[from@pos] <- from@value
        regular_vector
      })

#' Show Method for sparse_numeric
#'
#' Displays a sparse_numeric object in a human-readable summary.
#'
#' @name sparse_numeric-show
#' @rdname sparse_numeric-show
#'
#' @aliases show,sparse_numeric-method
#'
#' @param object A sparse_numeric object.
#'
#' @export
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

#' Plot Overlapping Non-Zero Elements of Two Sparse Vectors
#'
#' Visualizes the positions and values where two sparse_numeric vectors both
#' have non-zero entries.
#'
#' @name sparse_numeric-plot
#' @rdname sparse_numeric-plot
#'
#' @aliases plot,sparse_numeric,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param y A sparse_numeric object.
#' @param ... Additional graphical arguments passed to \code{plot()}.
#'
#' @return A scatterplot.
#' @export
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

#' Count Non-Zero Elements in a sparse_numeric Vector
#'
#' Returns the number of non-zero entries stored in a sparse_numeric object.
#'
#' @name count_sparse
#' @rdname count_sparse
#'
#' @aliases count_sparse count_sparse,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return An integer count.
#' @export
setGeneric ("count_sparse", function (x, ...) standardGeneric ("count_sparse"))
setMethod ("count_sparse", "sparse_numeric", function(x, ...) {
  length (x@pos)
})

#' Mean of a sparse_numeric Vector
#'
#' Computes the mean including zeros, without converting to dense form.
#'
#' @name sparse_numeric-mean
#' @rdname sparse_numeric-mean
#'
#' @aliases mean,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A numeric scalar.
#' @export
setMethod ("mean", "sparse_numeric",
           function (x, ...) {
             n <- as.numeric(x@length)
             if (n == 0) {
               return(NaN)
             }
             sum(x@value) / n
           })

#' Squared Norm of a sparse_numeric Vector
#'
#' Computes the Euclidean norm (L2 norm) of the sparse vector.
#'
#' @name norm
#' @rdname norm
#'
#' @aliases norm norm,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A numeric scalar.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))
setMethod ("norm", "sparse_numeric",
           function (x, ...) {
             sqrt (sum (x@value^2))
           })

#' Standardize a sparse_numeric Vector
#'
#' Produces a standardized sparse_numeric vector with mean zero and unit variance.
#'
#' @name standardize
#' @rdname standardize
#'
#' @aliases standardize standardize,sparse_numeric-method
#'
#' @param x A sparse_numeric object.
#' @param ... Additional arguments (unused).
#'
#' @return A sparse_numeric object.
#' @export
setGeneric ("standardize", function (x, ...) standardGeneric ("standardize"))
setMethod ("standardize", "sparse_numeric",
           function (x, ...) {
             n <- as.numeric (x@length)
             if (n == 0 || n == 1)
               stop ("Cannot standardize a sparse_numeric of length 0 or 1.")
             
             v  <- x@value
             p  <- x@pos
             
             # Mean 
             mu    <- sum(v) / n
             
             # Sum of squares 
             sum_x2 <- sum(v^2)
             
             
             # Sample variance: sum((x - mu)^2) / (n - 1)
             var_x <- (sum_x2 - n * mu^2) / (n - 1)
             sd_x  <- sqrt(var_x)
             
             if (sd_x == 0) {
               stop("Standard deviation is zero. Cannot standardize.")
             }
             
             # Build standardized full vector
             vals <- rep.int((-mu) / sd_x, x@length)  # positions that were 0
             if (length(p) > 0L) 
               vals[p] <- (v - mu) / sd_x
             
             # Find non-zero indices
             indices <- which(vals != 0)
             
             new("sparse_numeric",
                 value  = vals[indices],
                 pos    = as.integer(indices),
                 length = x@length)
           })
