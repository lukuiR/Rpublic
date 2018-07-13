# https://app.codility.com/demo/results/training85RHJE-ART/
def solution(N):
    s=str(bin(N))[2:]
    l=len(s)
    m=0
    x=1
    y=0
    for i in range(1,l):
        if(s[l-i]=="1"):
            x=0
            y=0
        if(x==0 and s[l-i]=="0"):
            y=y+1
            if(m<y):
                m=y
    return m
