#https://app.codility.com/demo/results/trainingDUCHPE-24C/
def solution(S, P, Q):
    # write your code in Python 3.6
    d4=[0]*len(S)
    d3=[0]*len(S)
    d2=[0]*len(S)
    d1=[0]*len(S)
    for i in range(len(S)):
        if S[i]=="A":
            d1[i]=1
        if S[i]=="C":
            d2[i]=1
        if S[i]=="G":
            d3[i]=1
        if S[i]=="T":
            d4[i]=1
    for i in range(len(S)-1):
        d1[i+1]=d1[i]+d1[i+1]
        d2[i+1]=d2[i]+d2[i+1]
        d3[i+1]=d3[i]+d3[i+1]
        d4[i+1]=d4[i]+d4[i+1]
    a=[0]*len(P)
    for i in range(len(P)):
        if P[i]>0 and Q[i]<len(S):
            if d1[Q[i]]-d1[P[i]-1]>0:
                a[i]=1
            elif d2[Q[i]]-d2[P[i]-1]>0:
                a[i]=2
            elif d3[Q[i]]-d3[P[i]-1]>0:
                a[i]=3
            elif d4[Q[i]]-d4[P[i]-1]>0:
                a[i]=4
        if P[i]==0 and Q[i]<len(S):
            if d1[Q[i]]>0:
                a[i]=1
            elif d2[Q[i]]>0:
                a[i]=2
            elif d3[Q[i]]>0:
                a[i]=3
            elif d4[Q[i]]>0:
                a[i]=4
        
    return a 
