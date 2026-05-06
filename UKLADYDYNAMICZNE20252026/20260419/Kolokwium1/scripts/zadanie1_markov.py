from collections import defaultdict
import random


ORIGINAL_MELODY = [
    "G4", "E4", "E4", "F4", "D4", "D4", "C4", "D4", "E4", "F4", "G4", "G4",
    "G4", "G4", "E4", "E4", "F4", "D4", "D4", "C4", "E4", "G4", "G4", "C4",
]

SEED = 20260420


def unique_states(melody):
    return sorted(set(melody), key=lambda note: (int(note[-1]), note[0]))


def transition_counts(melody, states):
    counts = {state: {target: 0 for target in states} for state in states}
    for current_note, next_note in zip(melody, melody[1:]):
        counts[current_note][next_note] += 1
    return counts


def transition_matrix(counts, states):
    matrix = {}
    for state in states:
        total = sum(counts[state].values())
        matrix[state] = {
            target: (counts[state][target] / total if total else 0.0)
            for target in states
        }
    return matrix


def generate_melody(start_note, matrix, length, seed):
    rng = random.Random(seed)
    melody = [start_note]
    current = start_note

    for _ in range(length - 1):
        row = matrix[current]
        targets = [note for note, prob in row.items() if prob > 0]
        weights = [row[note] for note in targets]
        current = rng.choices(targets, weights=weights, k=1)[0]
        melody.append(current)

    return melody


def print_nested_table(title, data, states):
    print(title)
    header = "     " + " ".join(f"{state:>8}" for state in states)
    print(header)
    for state in states:
        row = " ".join(f"{data[state][target]:>8}" for target in states)
        print(f"{state:>4} {row}")
    print()


def main():
    states = unique_states(ORIGINAL_MELODY)
    counts = transition_counts(ORIGINAL_MELODY, states)
    matrix = transition_matrix(counts, states)
    generated = generate_melody(
        start_note=ORIGINAL_MELODY[0],
        matrix=matrix,
        length=len(ORIGINAL_MELODY),
        seed=SEED,
    )

    print("Original melody:")
    print(ORIGINAL_MELODY)
    print()
    print("States:")
    print(states)
    print()
    print_nested_table("Transition counts:", counts, states)
    print_nested_table("Transition probabilities:", matrix, states)
    print(f"Generated melody for seed {SEED}:")
    print(generated)


if __name__ == "__main__":
    main()
