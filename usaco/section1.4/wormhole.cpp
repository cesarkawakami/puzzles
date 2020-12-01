/*
ID: cesarka2
LANG: C++14
TASK: wormhole
*/
#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <vector>

using Point = std::pair<int, int>;

int N;
std::vector<Point> points;
std::vector<int> x_transitions;

int combinations;
std::vector<int> pair;

std::string to_string(Point p) {
    std::ostringstream ss;
    ss << "(" << p.first << ", " << p.second << ")";
    return ss.str();
}

bool cycles_if_starting_at(int v) {
    int previous = -1;

    for (int i = 0; i < 100; ++i) {
        // std::cout << to_string(points[v]) << " ";
        if (pair[v] != previous) {
            previous = v;
            v = pair[v];
            continue;
        } else if (x_transitions[v] != -1) {
            previous = -1;
            v = x_transitions[v];
            continue;
        }
        return false;
    }
    // std::cout << "\n";

    return true;
}

bool has_cycle() {
    for (int v = 0; v < N; ++v) {
        // std::cout << "detecting cycle starting at " << to_string(points[v]) << "\n";
        if (cycles_if_starting_at(v)) {
            return true;
        }
    }
    return false;
}

void go() {
    bool pair_found = false;
    for (int i = 0; i < N; ++i) {
        if (pair[i] != -1) {
            continue;
        }
        for (int j = i + 1; j < N; ++j) {
            if (pair[j] != -1) {
                continue;
            }
            pair_found = true;
            pair[i] = j;
            pair[j] = i;
            go();
            pair[i] = pair[j] = -1;
        }
        break;
    }

    if (!pair_found) {
        // std::cout << ">>> pairings:\n";
        // std::set<int> seen;
        // for (int v = 0; v < N; ++v) {
        //     if (seen.count(v)) {
        //         continue;
        //     }
        //     seen.insert(v);
        //     seen.insert(pair[v]);
        //     std::cout << points[v].first << "," << points[v].second << " -- "
        //               << points[pair[v]].first << "," << points[pair[v]].second << "\n";
        // }

        if (has_cycle()) {
            // std::cout << "has cycle\n";
            ++combinations;
        }
        return;
    }
}

int main() {
#ifndef LOCAL
    std::freopen("wormhole.in", "r", stdin);
    std::freopen("wormhole.out", "w", stdout);
#endif

    std::cin >> N;

    points.reserve(N);
    x_transitions.resize(N);
    std::fill(x_transitions.begin(), x_transitions.end(), -1);
    pair.resize(N);
    std::fill(pair.begin(), pair.end(), -1);

    for (int i = 0; i < N; ++i) {
        int x, y;
        std::cin >> x >> y;
        points.push_back({x, y});
    }
    for (int from = 0; from < N; ++from) {
        int best_id = -1;
        for (int to = 0; to < N; ++to) {
            if (from != to && points[from].second == points[to].second &&
                points[to].first > points[from].first &&
                (best_id == -1 || points[to].first < points[best_id].first)) {
                best_id = to;
            }
        }
        x_transitions[from] = best_id;
    }

    combinations = 0;
    go();
    std::cout << combinations << "\n";
}
