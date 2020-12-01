/*
ID: cesarka2
LANG: C++14
TASK: hidden
*/
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <ios>
#include <iostream>
#include <string>
#include <vector>

const std::uint64_t prime = 1e9 + 7;
std::string super;
std::vector<std::uint64_t> hashes;

std::uint64_t primepow(int x) {
    std::uint64_t res = 1, pow = prime;
    while (x) {
        if (x & 1) {
            res *= pow;
        }
        x >>= 1;
        pow *= pow;
    }
    return res;
}

std::uint64_t hash(int start, int end) {
    assert(end >= start);
    std::uint64_t a = (start == 0) ? 0 : hashes[start - 1];
    std::uint64_t b = (end == 0) ? 0 : hashes[end - 1];
    return b - a * primepow(end - start);
}

struct Substr {
    int id;
    int start, end;

    int length() const { return end - start; }
    std::uint64_t hash(int h_start, int h_end) const {
        assert(h_end <= end - start);
        return ::hash(start + h_start, start + h_start + h_end);
    }
    char at(int pos) const {
        assert(pos < end - start);
        return super[start + pos];
    }
};

int lcp(Substr a, Substr b) {
    int left = 0, right = std::min(a.length(), b.length());
    while (left < right) {
        int mid = (left + right + 1) / 2;
        if (a.hash(0, mid) == b.hash(0, mid)) {
            left = mid;
        } else {
            right = mid - 1;
        }
    }
    return left;
}

bool operator<(Substr a, Substr b) {
    int m = lcp(a, b);
    if (a.length() == m || b.length() == m) {
        return a.length() < b.length();
    }
    return a.at(m) < b.at(m);
}

std::vector<Substr> rotations;

int main() {
#ifndef LOCAL
    std::freopen("hidden.in", "r", stdin);
    std::freopen("hidden.out", "w", stdout);
#endif

    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int L;
    std::cin >> L;
    while ((int)super.length() < L) {
        std::string buf;
        std::cin >> buf;
        super += buf;
    }
    super += super.substr(0, L - 1);

    hashes.resize(super.length());
    std::uint64_t prev_hash = 0;
    for (int i = 0; i < 2 * L - 1; ++i) {
        prev_hash = hashes[i] = prev_hash * prime + super[i];
    }

    rotations.reserve(L);
    for (int i = 0; i < L; ++i) {
        rotations.push_back({i, i, i + L});
    }

    int min_rotation_id = std::min_element(rotations.begin(), rotations.end())->id;
    std::cout << min_rotation_id << "\n";
}
