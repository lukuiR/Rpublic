# https://app.codility.com/demo/results/training6HGZ8R-W55/
def solution(A):
    # write your code in Python 3.6
    B=[0]*len(A)
    for i in A:
        if(i>0 and i<len(A)+1):
            B[i-1]=1
    if (sum(B)==len(A)):
        return 1
    else:
        return 0
