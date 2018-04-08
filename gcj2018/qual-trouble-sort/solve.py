def trouble_sort(arr):
    odd, even = arr[::2], arr[1::2]
    odd, even = sorted(odd), sorted(even)
    rv = arr[:]
    rv[::2] = odd
    rv[1::2] = even
    return rv


def main():
    case_count = int(input())
    for case_number in range(1, case_count + 1):
        print('Case #{}:'.format(case_number), end=' ')
        n = int(input())
        arr = list(map(int, input().split()))
        new_arr = trouble_sort(arr)
        for i in range(len(new_arr) - 1):
            if new_arr[i] > new_arr[i + 1]:
                print(i)
                break
        else:
            print('OK')


if __name__ == '__main__':
    main()
