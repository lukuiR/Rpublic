#https://app.codility.com/demo/results/trainingVGYRGM-37P/

def solution(A):
    # write your code in Python 3.6
    l=len(A)
    s=(l+1)*(l+2)
    s=s//2
    for i in A:
        s-=i
    return s
