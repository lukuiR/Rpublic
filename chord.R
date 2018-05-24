#https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html


library(circlize)
# Create a random adjacency matrix
#


set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
mat

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)
chordDiagram(mat)
