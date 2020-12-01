/*
ID: cesarka2
LANG: C++14
TASK: telecow
*/
#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <iterator>
#include <ostream>
#include <sched.h>
#include <set>
#include <utility>
#include <vector>

struct CowputerId {
    int id;

    bool is_extremal() const;
    int in_node() const { return 2 * (id - 1); }
    int out_node() const { return 2 * (id - 1) + 1; }
};

std::istream &operator>>(std::istream &in, CowputerId &cowputer_id) {
    in >> cowputer_id.id;
    return in;
}

std::ostream &operator<<(std::ostream &out, const CowputerId cowputer_id) {
    out << cowputer_id.id;
    return out;
}

int N, M;
CowputerId c1, c2;
std::vector<CowputerId> all_cowputer_ids;
std::vector<std::vector<int>> neighbors;
std::vector<std::vector<int>> capacities;
std::vector<std::vector<int>> residual;

bool CowputerId::is_extremal() const { return id == c1.id || id == c2.id; }

bool find_path(int s, int t, std::vector<int> &path) {
    path.clear();

    static std::vector<char> seen;
    seen.resize(neighbors.size());
    std::fill(seen.begin(), seen.end(), false);

    static std::vector<int> to_visit;
    to_visit.reserve(neighbors.size());
    to_visit = {s};
    seen[s] = true;

    static std::vector<int> previous;
    previous.resize(neighbors.size());
    std::fill(previous.begin(), previous.end(), -0x3f3f3f3f);

    bool found = false;
    while (!to_visit.empty()) {
        int v = to_visit.back();
        if (v == t) {
            found = true;
            break;
        }
        to_visit.pop_back();
        for (int neighbor : neighbors[v]) {
            if (!seen[neighbor] && residual[v][neighbor] > 0) {
                to_visit.push_back(neighbor);
                seen[neighbor] = true;
                previous[neighbor] = v;
            }
        }
    }

    if (found) {
        int v = t;
        while (v != s) {
            // std::cout << v << std::endl;
            path.push_back(v);
            v = previous[v];
        }
        path.push_back(s);
        std::reverse(path.begin(), path.end());
        return true;
    }
    return false;
}

void push_flow(const std::vector<int> &path) {
    for (int i = 0; i < (int)path.size() - 1; ++i) {
        int v = path[i], next = path[i + 1];
        assert(residual[v][next] > 0);
        --residual[v][next];
        ++residual[next][v];
    }
}

void add_capacity(int a, int b, int c) {
    const auto add_to_vector = [](std::vector<int> &v, int x) {
        if (std::find(v.begin(), v.end(), x) == v.end()) {
            v.push_back(x);
        }
    };
    add_to_vector(neighbors[a], b);
    add_to_vector(neighbors[b], a);
    capacities[a][b] += c;
    residual[a][b] += c;
}

void zero_out_capacity(int a, int b) {
    capacities[a][b] = capacities[b][a] = residual[a][b] = residual[b][a] = 0;
}

int flow(int a, int b) { return capacities[a][b] - residual[a][b]; }

template <typename T> std::vector<T> reserved_vec(int n) {
    std::vector<T> v;
    v.reserve(n);
    return v;
}

int main() {
#ifndef LOCAL
    std::freopen("telecow.in", "r", stdin);
    std::freopen("telecow.out", "w", stdout);
#endif

    std::cin >> N >> M >> c1 >> c2;

    for (int i = 0; i < N; ++i) {
        all_cowputer_ids.push_back(CowputerId{i + 1});
    }
    neighbors = std::vector<std::vector<int>>(2 * N, reserved_vec<int>(10));
    capacities = std::vector<std::vector<int>>(2 * N, std::vector<int>(2 * N));
    residual = std::vector<std::vector<int>>(2 * N, std::vector<int>(2 * N));

    for (auto cowputer_id : all_cowputer_ids) {
        if (!cowputer_id.is_extremal()) {
            add_capacity(cowputer_id.in_node(), cowputer_id.out_node(), 1);
        }
    }

    for (int i = 0; i < M; ++i) {
        CowputerId a, b;
        std::cin >> a >> b;
        add_capacity(a.out_node(), b.in_node(), 1e6);
        add_capacity(b.out_node(), a.in_node(), 1e6);
    }

    // std::cout << out_node(c1) << " " << in_node(c2) << std::endl;
    // for (int i = 0; i < (int)capacities.size(); ++i) {
    //     std::string spc = "";
    //     for (int j = 0; j < (int)capacities[i].size(); ++j) {
    //         std::cout << spc << capacities[i][j];
    //         spc = " ";
    //     }
    //     std::cout << std::endl;
    // }

    int max_flow = 0;
    {
        std::vector<int> path;
        path.reserve(neighbors.size());
        while (find_path(c1.out_node(), c2.in_node(), path)) {
            // std::cout << "found:";
            // std::copy(path.begin(), path.end(), std::ostream_iterator<int>(std::cout, " "));
            // std::cout << std::endl;
            ++max_flow;
            push_flow(path);
        }
    }

    std::vector<CowputerId> required_cowputers;
    for (auto cow : all_cowputer_ids) {
        if (cow.is_extremal()) {
            continue;
        }
        assert(capacities[cow.in_node()][cow.out_node()] == 1 &&
               capacities[cow.out_node()][cow.in_node()] == 0);

        if (flow(cow.in_node(), cow.out_node()) == 0) {
            continue;
        }

        zero_out_capacity(cow.in_node(), cow.out_node());

        std::vector<int> path;
        if (find_path(cow.in_node(), cow.out_node(), path)) {
            push_flow(path);
            add_capacity(cow.in_node(), cow.out_node(), 1);
            continue;
        }

        add_capacity(c1.out_node(), c2.in_node(), 1);
        assert(find_path(cow.in_node(), cow.out_node(), path));
        push_flow(path);
        zero_out_capacity(c1.out_node(), c2.in_node());
        required_cowputers.push_back(cow);
    }

    std::cout << max_flow << std::endl;
    // std::cout << "required nodes: " << required_nodes.size() << std::endl;
    assert((int)required_cowputers.size() == max_flow);
    std::string space = "";
    for (auto cow : required_cowputers) {
        std::cout << space << cow;
        space = " ";
    }
    std::cout << "\n";
}
