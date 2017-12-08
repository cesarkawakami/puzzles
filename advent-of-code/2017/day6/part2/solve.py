from sys import stdin

arr = [int(x) for x in stdin.read().split()]

def cycle(arr):
    current_index, acc = max(enumerate(arr), key=lambda x: (x[1], -x[0]))
    arr[current_index] = 0
    current_index += 1
    if current_index >= len(arr):
        current_index = 0
    while acc:
        arr[current_index] += 1
        acc -= 1
        current_index += 1
        if current_index >= len(arr):
            current_index = 0

seen = {
    tuple(arr): 0,
}
count = 0
while True:
    count += 1
    cycle(arr)
    if tuple(arr) in seen:
        print(count - seen[tuple(arr)])
        break
    seen[tuple(arr)] = count

# print(count)