"""https://app.codility.com/demo/results/training7J6H55-YHF/
"""
def solution(A):
    # write your code in Python 3.6
    n=A[0]
    z=len(A)-1
    for i in range(z):
        n^=A[i+1]
    return n
