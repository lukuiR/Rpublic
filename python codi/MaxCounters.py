# https://app.codility.com/demo/results/training7S93QS-K27/
def solution(N, A):
    # write your code in Python 3.6
    lm=0
    mm=0
    B=[0]*N
    for i in A:
        if i>N:
            lm=mm
        else:
            if B[i-1]<lm:
                B[i-1]=lm
                B[i-1]+=1
                if B[i-1]>mm:
                    mm+=1
            else:
                B[i-1]+=1
                if B[i-1]>mm:
                    mm+=1
    for i in range(N):
        B[i]=max(B[i],lm)
    return B
