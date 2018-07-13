"""https://app.codility.com/demo/results/training4HRP4B-9FZ/
"""
def solution(A, K):
    l=len(A)
    B=[0]*l
    for i in range(l):
        t=i+K
        t=t%l
        B[t]=A[i]
    return B
