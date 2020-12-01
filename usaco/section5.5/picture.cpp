/*
ID: cesarka2
LANG: C++14
TASK: picture
*/
#include <algorithm>
#include <bits/stdint-intn.h>
#include <cassert>
#include <deque>
#include <exception>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <tuple>
#include <utility>
#include <vector>

int N;

struct SegTree;

std::vector<SegTree> seg_tree;

struct SegTree {
    int min, max;
    int left_id, right_id;
    int delta;
    int zeros;

    SegTree &left() const {
        assert(left_id >= 0);
        return seg_tree[left_id];
    }
    SegTree &right() const {
        assert(right_id >= 0);
        return seg_tree[right_id];
    }

    void add(int add_min, int add_max, int add_delta) {
        if (add_max <= min || add_min >= max) {
            return;
        }
        if (add_min <= min && max <= add_max) {
            delta += add_delta;
            update_zeros();
            return;
        }
        left().add(add_min, add_max, add_delta);
        right().add(add_min, add_max, add_delta);
        update_zeros();
    }

    int update_zeros() {
        if (delta > 0) {
            zeros = 0;
        } else if (left_id >= 0) {
            assert(right_id >= 0);
            zeros = left().zeros + right().zeros;
        } else {
            zeros = max - min;
        }
        return zeros;
    }

    int count_zeros(int count_min, int count_max) const {
        if (count_max <= min || count_min >= max) {
            return 0;
        }
        if (count_min <= min && max <= count_max) {
            return zeros;
        }
        if (delta > 0) {
            return 0;
        }
        return left().count_zeros(count_min, count_max) + right().count_zeros(count_min, count_max);
    }
};

int make_node(int min, int max) {
    int id = (int)seg_tree.size();
    seg_tree.push_back({min, max, -1, -1, 0, max - min});
    return id;
}

SegTree &build_tree(int min, int max) {
    int root_id = make_node(min, max);
    std::deque<int> to_process{root_id};
    while (!to_process.empty()) {
        auto &node = seg_tree[to_process.front()];
        to_process.pop_front();
        if (node.max - node.min == 1) {
            continue;
        }
        int mid = (node.min + node.max) / 2;
        node.left_id = make_node(node.min, mid);
        to_process.push_back(node.left_id);
        node.right_id = make_node(mid, node.max);
        to_process.push_back(node.right_id);
    }
    return seg_tree[root_id];
}

enum struct EventType : int8_t {
    ENTER,
    EXIT,
};

std::vector<std::tuple<int, EventType, int, int>> events;

std::tuple<int, int, int, int> rotate(const std::tuple<int, int, int, int> &r) {
    int rx1, ry1, rx2, ry2;
    std::tie(rx1, ry1, rx2, ry2) = r;

    int nx1 = -ry1, ny1 = rx1, nx2 = -ry2, ny2 = rx2;
    int llx, lly, urx, ury;
    std::tie(llx, urx) = std::minmax(nx1, nx2);
    std::tie(lly, ury) = std::minmax(ny1, ny2);
    return {llx, lly, urx, ury};
}

int main() {
#ifndef LOCAL
    std::freopen("picture.in", "r", stdin);
    std::freopen("picture.out", "w", stdout);
#endif

    std::cin >> N;
    seg_tree.reserve(1e5);
    events.reserve(2e4);;

    std::vector<std::tuple<int, int, int, int>> rectangles;
    for (int i = 0; i < N; ++i) {
        int llx, lly, urx, ury;
        std::cin >> llx >> lly >> urx >> ury;
        rectangles.push_back({llx, lly, urx, ury});
    }

    int total_perimeter = 0;
    for (int ctr = 0; ctr < 2; ++ctr) {
        int miny = std::numeric_limits<int>::max(), maxy = std::numeric_limits<int>::min();
        for (const auto &rect : rectangles) {
            int llx, lly, urx, ury;
            std::tie(llx, lly, urx, ury) = rect;
            miny = std::min(miny, lly);
            maxy = std::max(maxy, ury);
        }

        seg_tree.clear();
        auto &root = build_tree(miny, maxy);

        events.clear();
        for (const auto &rect : rectangles) {
            int llx, lly, urx, ury;
            std::tie(llx, lly, urx, ury) = rect;
            events.push_back({llx, EventType::ENTER, lly, ury});
            events.push_back({urx, EventType::EXIT, lly, ury});
        }
        std::sort(events.begin(), events.end());

        int axis_perimeter = 0;
        for (const auto &event : events) {
            int x, miny, maxy;
            EventType event_type;
            std::tie(x, event_type, miny, maxy) = event;
            if (event_type == EventType::ENTER) {
                axis_perimeter += root.count_zeros(miny, maxy);
                root.add(miny, maxy, +1);
            } else if (event_type == EventType::EXIT) {
                root.add(miny, maxy, -1);
                axis_perimeter += root.count_zeros(miny, maxy);
            } else {
                throw std::logic_error("invalid event type");
            }
        }
        total_perimeter += axis_perimeter;

        for (auto &r : rectangles) {
            r = rotate(r);
        }
    }

    std::cout << total_perimeter << "\n";
}
