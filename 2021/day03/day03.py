import numpy as np
from numpy.lib.function_base import copy

with open('03_input', 'r') as f:
    data = f.readlines()
    data = [[int(i) for i in s.strip('\n')] for s in data]
    data = np.array(data)

gamma = ''
epsilon = ''

mid = data.shape[0] / 2

for i in range(data.shape[1]):
    ones = np.count_nonzero(data[:, i])
    if ones > mid:
        gamma += '1'
        epsilon += '0'
    elif ones < mid:
        gamma += '0'
        epsilon += '1'


def bin_list_to_dec(l):
    print(l)
    s = ''
    s = ''.join([s] + [str(e) for e in l])
    return int(s, 2)

# most or 1
def oxy(lst):
    data = np.copy(lst)
    for i in range(data.shape[1]):
        # how many ones?
        ones = np.count_nonzero(data[:, i])
        zeroes = data.shape[0] - ones

        tokeep = 1 if ones >= zeroes else 0
        data = data[data[:, i] == tokeep]

        if data.shape[0] == 1:
            return bin_list_to_dec(data[0])

# least or 0
def co2(lst):
    data = np.copy(lst)
    for i in range(data.shape[1]):
        ones = np.count_nonzero(data[:, i])
        zeroes = data.shape[0] - ones
        
        tokeep = 0 if zeroes <= ones else 1
        data = data[data[:, i] == tokeep]

        if data.shape[0] == 1:
            return bin_list_to_dec(data[0])


print(f'sol: {gamma} + {epsilon} = {int(gamma, 2) * int(epsilon, 2)}')

o = oxy(data)
co = co2(data)
print(f'hm: {o} {co} : {o * co}')
