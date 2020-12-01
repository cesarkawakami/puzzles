/*
ID: cesarka2
LANG: C++14
TASK: skidesign
*/
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

int sq(int x) { return x * x; }

int main() {
#ifndef LOCAL
    std::freopen("skidesign.in", "r", stdin);
    std::freopen("skidesign.out", "w", stdout);
#endif

    int N;
    std::cin >> N;

    std::vector<int> hills;
    std::copy_n(std::istream_iterator<int>(std::cin), N, std::back_inserter(hills));

    int best = -1;
    for (int min_elev = 0; min_elev + 17 <= 100; ++min_elev) {
        int max_elev = min_elev + 17;
        int cost = 0;
        for (int h : hills) {
            if (h < min_elev) {
                cost += sq(min_elev - h);
            } else if (h > max_elev) {
                cost += sq(h - max_elev);
            }
        }
        if (best == -1 || cost < best) {
            best = cost;
        }
    }
    std::cout << best << "\n";
}
