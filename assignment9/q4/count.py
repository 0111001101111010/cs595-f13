#open the file to count the number of iterations

with open('num.txt') as fin:
    lines = sum(1 for line in fin)

print lines