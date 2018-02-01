#https://www.youtube.com/watch?v=3GEFd8rZQgY
library("R6")
Stack <- R6Class("Stack",
                 public = list(
                   push = function(x) {
                     private$items[[self$size()+1]]<-x
                     invisible(self)
                   },
                   pop = function() {
                     if (self$size() == 0)
                       stop("No more items on stack")
                     item <- private$items[[self$size()]]
                     private$items <- private$items[-self$size()]
                     item
                   },
                   size = function() {
                     length(private$items)
                   }
                 ),
                 private = list(
                   items = list()
                 )
  
)

s <- Stack$new()
s$push(1)
s$push(2:8)
s$push("aa")

s$pop()
x<-s$pop()
