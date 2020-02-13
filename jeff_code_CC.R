library(igraph)
g <- make_graph( ~ A-B-C-D-A, E-A:B:C:D,
                 F-G-H-I-F, J-F:G:H:I,
                 K-L-M-N-K, O-K:L:M:N,
                 P-Q-R-S-P, T-P:Q:R:S,
                 B-F, E-J, C-I, L-T, O-T, M-S,
                 C-P, C-L, I-L, I-P)

plot(g)


g <- make_graph( ~ A-B-C-D-A)
g <- make_graph( ~ E-A:B:C:D, A-C:B:D, C-B:D, D-B)
g <- make_graph( ~ E-A:B:C:D, A-C:B:D, C-B:D, D-B
                 , F-G:H:I, G-H:I, H-I
                 , J-K:L:M, K-L:M, L-M
                 , D-L:J:M:K)
plot(g)
