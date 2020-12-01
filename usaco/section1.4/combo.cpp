/*
ID: cesarka2
LANG: C++14
TASK: combo
*/
#include <algorithm>
#include <cstdio>
#include <fstream>
#include <ios>
#include <iostream>
#include <iterator>
#include <vector>

int main() {
#ifndef LOCAL
    std::freopen("combo.in", "r", stdin);
    std::freopen("combo.out", "w", stdout);
#endif

    int N;
    std::cin >> N;

    const int close_limit = 2;

    auto close_enough = [N](int a, int b) {
        int x = std::min(std::abs(a - b), std::min(std::abs(a - b - N), std::abs(a - b + N)));
        return x <= close_limit;
    };

    const int K = 3;

    std::vector<int> john, master;
    std::copy_n(std::istream_iterator<int>(std::cin), K, std::back_inserter(john));
    std::copy_n(std::istream_iterator<int>(std::cin), K, std::back_inserter(master));

    int answer = 0;
    for (int i = 1; i <= N; ++i) {
        for (int j = 1; j <= N; ++j) {
            for (int k = 1; k <= N; ++k) {
                if ((close_enough(i, john[0]) && close_enough(j, john[1]) && close_enough(k, john[2]))
                || (close_enough(i, master[0]) && close_enough(j, master[1]) && close_enough(k, master[2]))) {
                    ++answer;
                }
            }
        }
    }
    // int answer = 1;
    // for (int i = 0; i < K; ++i) {
    //     int valid_values = 0;
    //     for (int j = 1; j <= N; ++j) {
    //         if (close_enough(j, john[i]) || close_enough(j, master[i])) {
    //             ++valid_values;
    //         }
    //     }
    //     answer *= valid_values;
    // }

    std::cout << answer << "\n";
}
