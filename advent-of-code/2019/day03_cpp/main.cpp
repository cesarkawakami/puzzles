#include <algorithm>
#include <benchmark/benchmark.h>
#include <bits/c++config.h>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

enum struct Dir { R, L, D, U };

struct Move {
    Dir dir;
    int d;
};

struct Coord {
    int x;
    int y;
    auto operator<=>(const Coord &) const = default;

    int manh() const { return abs(x) + abs(y); }
};

void hash_combine(std::size_t &seed, const std::size_t other) {
    // Ripped from boost
    seed ^= other + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template <> struct std::hash<Coord> {
    std::size_t operator()(const Coord &c) const {
        std::size_t result{0};
        std::hash<int> int_hash;
        hash_combine(result, int_hash(c.x));
        hash_combine(result, int_hash(c.y));
        return result;
    }
};

std::tuple<int, int> delta_of(const Dir &dir) {
    if (dir == Dir::R) {
        return {1, 0};
    } else if (dir == Dir::L) {
        return {-1, 0};
    } else if (dir == Dir::U) {
        return {0, 1};
    } else if (dir == Dir::D) {
        return {0, -1};
    } else {
        throw 0;
    }
}

std::vector<Coord> wire_coords(const std::vector<Move> &moves) {
    int x = 0, y = 0;
    std::vector<Coord> output;
    for (const auto &move : moves) {
        auto [dx, dy] = delta_of(move.dir);
        for (int i = 0; i < move.d; i++) {
            x += dx;
            y += dy;
            output.push_back({x, y});
        }
    }
    return output;
}

std::vector<Coord> wire_inters(const std::vector<Move> &moves1, const std::vector<Move> &moves2) {
    std::vector<Coord> coords1 = wire_coords(moves1);
    std::vector<Coord> coords2 = wire_coords(moves2);
    std::sort(coords1.begin(), coords1.end());
    std::sort(coords2.begin(), coords2.end());

    std::vector<Coord> output;
    std::set_intersection(coords1.begin(), coords1.end(), coords2.begin(), coords2.end(),
                          std::back_inserter(output));
    return output;
}

template <typename F> void wire_coords_gen(const std::vector<Move> &moves, const F &f) {
    int x = 0, y = 0;
    for (const auto &move : moves) {
        auto [dx, dy] = delta_of(move.dir);
        for (int i = 0; i < move.d; i++) {
            x += dx;
            y += dy;
            f({x, y});
        }
    }
}

std::vector<Coord> wire_inters_hashset(const std::vector<Move> &moves1,
                                       const std::vector<Move> &moves2) {
    std::unordered_set<Coord> coords1;
    wire_coords_gen(moves1, [&](Coord c) { coords1.insert(c); });

    std::vector<Coord> output;
    wire_coords_gen(moves2, [&](Coord c) {
        if (coords1.contains(c)) {
            output.push_back(c);
        }
    });

    return output;
}

std::vector<Coord> wire_inters_set(const std::vector<Move> &moves1,
                                   const std::vector<Move> &moves2) {
    std::set<Coord> coords1;
    wire_coords_gen(moves1, [&](Coord c) { coords1.insert(c); });

    std::vector<Coord> output;
    wire_coords_gen(moves2, [&](Coord c) {
        if (coords1.contains(c)) {
            output.push_back(c);
        }
    });

    return output;
}

Move parse_move(const std::string &s) {
    int d{std::stoi(s.substr(1))};
    Dir dir;
    if (s[0] == 'R') {
        dir = Dir::R;
    } else if (s[0] == 'L') {
        dir = Dir::L;
    } else if (s[0] == 'D') {
        dir = Dir::D;
    } else if (s[0] == 'U') {
        dir = Dir::U;
    } else {
        throw "parse error";
    }
    return Move{dir, d};
}

std::vector<Move> parse_wire(const std::string &s) {
    std::istringstream ss(s);
    std::vector<Move> output;
    while (true) {
        std::string move_str;
        std::getline(ss, move_str, ',');
        if (move_str.empty()) {
            break;
        }
        output.push_back(parse_move(move_str));
    }
    return output;
}

std::tuple<std::vector<Move>, std::vector<Move>> parse_input(const std::string &s) {
    std::istringstream ss(s);
    std::string moves1, moves2;
    ss >> moves1 >> moves2;
    return {parse_wire(moves1), parse_wire(moves2)};
}

void do_part1_baseline() {
    std::ifstream in("input");
    std::string input(std::istreambuf_iterator<char>(in), {});
    auto [moves1, moves2] = parse_input(input);
    std::vector<Coord> common_coords = wire_inters(moves1, moves2);
    Coord c = *std::min_element(common_coords.begin(), common_coords.end(),
                                [](auto a, auto b) { return a.manh() < b.manh(); });
    std::cout << "part1, dist: " << c.manh() << std::endl;
}

void do_part1_hashset() {
    std::ifstream in("input");
    std::string input(std::istreambuf_iterator<char>(in), {});
    auto [moves1, moves2] = parse_input(input);
    std::vector<Coord> common_coords = wire_inters_hashset(moves1, moves2);
    Coord c = *std::min_element(common_coords.begin(), common_coords.end(),
                                [](auto a, auto b) { return a.manh() < b.manh(); });
    std::cout << "part1, dist: " << c.manh() << std::endl;
}

void do_part1_set() {
    std::ifstream in("input");
    std::string input(std::istreambuf_iterator<char>(in), {});
    auto [moves1, moves2] = parse_input(input);
    std::vector<Coord> common_coords = wire_inters_set(moves1, moves2);
    Coord c = *std::min_element(common_coords.begin(), common_coords.end(),
                                [](auto a, auto b) { return a.manh() < b.manh(); });
    std::cout << "part1, dist: " << c.manh() << std::endl;
}

static void BM_Part1_Baseline(benchmark::State &state) {
    for (auto _ : state) {
        do_part1_baseline();
    }
}
BENCHMARK(BM_Part1_Baseline)->Unit(benchmark::kMicrosecond);

static void BM_Part1_Hashset(benchmark::State &state) {
    for (auto _ : state) {
        do_part1_hashset();
    }
}
BENCHMARK(BM_Part1_Hashset)->Unit(benchmark::kMicrosecond);

static void BM_Part1_Set(benchmark::State &state) {
    for (auto _ : state) {
        do_part1_set();
    }
}
BENCHMARK(BM_Part1_Set)->Unit(benchmark::kMicrosecond);

BENCHMARK_MAIN();
