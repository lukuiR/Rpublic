""" https://app.codility.com/demo/results/trainingSBCKQR-6Z9/
"""

def solution(X, A):
    # write your code in Python 3.6
    x=[0]*X
    s=0
    for i in range(0,len(A)):
        if A[i]<X+1:
            if x[A[i]-1]==0:
                x[A[i]-1]=1
                s+=1
                
                if s==X:
                    return i
                    break
    return -1
