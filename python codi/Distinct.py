#https://app.codility.com/demo/results/training76QMJ9-7RE/

def solution(A):
    if len(A) == 0:
        return 0
 
    A.sort()
 
    nr_values = 1
 
    for i in range(1, len(A)):
        if A[i] != A[i-1]:
            nr_values += 1
 
    return nr_values
