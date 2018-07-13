#https://app.codility.com/demo/results/trainingVCZ9H8-883/

def solution(X, Y, D):
    # write your code in Python 3.6
    z=Y-X
    if Y < X or D <= 0:
        raise Exception("Invalid arguments")
    if z%D==0:
        return z//D
    else:
        return z//D+1
