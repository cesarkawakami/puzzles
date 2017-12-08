import re
from collections import Counter
from sys import stdin

graph = {}
for line in stdin:
    m = re.match(r'(\w+) \((\d+)\)( -> (.*))?', line)
    groups = m.groups()
    name = groups[0]
    weight = int(groups[1])
    if groups[3] is None:
        children = []
    else:
        children = [x.strip() for x in groups[3].split(',')]
    graph[name] = {
        'name': name,
        'weight': weight,
        'children': children,
    }


memo = {}
def solve(node):
    if node['name'] in memo:
        return memo[node['name']]

    weight_set = Counter()
    total_children_weight = 0
    for child in node['children']:
        child_weight = solve(graph[child])
        weight_set[child_weight] += 1
        total_children_weight += child_weight

    if len(weight_set) >= 2:
        if len(node['children']) == 2:
            raise RuntimeError('dont know how to solve this case')
        for key, value in weight_set.items():
            if value == 1:
                minority = key
            else:
                majority = key
        delta = majority - minority
        for child in node['children']:
            if solve(graph[child]) == minority:
                print(graph[child]['weight'] + delta)
                raise RuntimeError('solution found')
    elif len(weight_set) in (0, 1):
        pass
    else:
        raise RuntimeError('unreachable')

    rv = node['weight'] + total_children_weight
    memo[node['name']] = rv
    print(node, rv)
    return rv
        
for key, value in graph.items():
    solve(value)