#https://app.codility.com/demo/results/trainingDVZWUV-ETN/


def solution(A, B, K):
    # write your code in Python 3.6
    if A%K==0:
        return (B-A)//K+1
    else:
        return (B-A+A%K)//K
