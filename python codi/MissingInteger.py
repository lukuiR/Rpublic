def solution(A):
    # write your code in Python 3.6
    B=[0]*len(A)
    z=len(A)+1
    for i in range(0,len(A)):
        if A[i]>0 and A[i]<len(A)+1:
            B[A[i]-1]=1
    for i in range(0,len(A)):
        if B[i]==0:
            return i+1
            break
    return z
