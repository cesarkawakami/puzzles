#!/usr/bin/env python3

from sys import stdin
from typing import Dict, List, NewType, Set


Graph = NewType('Graph', Dict[int, List[int]])


def get_connected_vertices(
    graph: Graph,
    seen: Set[int],
    start: int,
) -> Set[int]:
    if start in seen:
        return set()

    new_seen = seen | {start}
    rv: Set[int] = {start}
    for neighbor in graph[start]:
        rv |= get_connected_vertices(graph, new_seen, neighbor)
    return rv


def main() -> None:
    graph = Graph({})
    for line in stdin:
        vertex_str, _, neighbor_list = line.split(None, 2)
        vertex = int(vertex_str)
        neighbors_str = neighbor_list.split(', ')
        neighbors = [int(x) for x in neighbors_str]
        graph[vertex] = neighbors
    connected_vertices = get_connected_vertices(graph, set(), 0)

    connected_components = set()
    for vertex in graph:
        connected_component = get_connected_vertices(graph, set(), vertex)
        connected_components.add(frozenset(connected_component))

    print(len(connected_components))


if __name__ == '__main__':
    main()
