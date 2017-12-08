from collections import defaultdict
from sys import stdin

OPERATOR_TO_FUNC = {
    '<': lambda a, b: a < b,
    '<=': lambda a, b: a <= b,
    '==': lambda a, b: a == b,
    '>': lambda a, b: a > b,
    '>=': lambda a, b: a >= b,
    '!=': lambda a, b: a != b,
}

registers = defaultdict(int)
current_answer = 0
for line in stdin:
    (
        change_register, 
        change_direction, 
        change_delta, 
        _,
        condition_register,
        condition_operator,
        condition_value,
    ) = line.split()
    change_delta, condition_value = int(change_delta), int(condition_value)

    if OPERATOR_TO_FUNC[condition_operator](
        registers[condition_register],
        condition_value,
    ):
        if change_direction == 'dec':
            change_delta = -change_delta
        registers[change_register] += change_delta

    current_answer = max(current_answer, max(registers.values()))

print(current_answer)