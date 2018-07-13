#https://app.codility.com/demo/results/trainingPEMJCC-7WA/

def solution(A):
    
    # write your code in Python 3.6
    l=len(A)-1
    s=sum(A)-A[0]
    r=A[0]
    m=abs(s-r)
    for i in range(l-1):
        s-=A[i+1]
        r+=A[i+1]
        a=abs(s-r)
        if m>a:
            m=a
    return m
