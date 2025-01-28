def main():
    with open("movement") as input_file: #Instead of Scanner
        direction = "north"
        horizontal = 0
        vertical = 0
        locations_visited = set() #keeps track of locations
        locations_visited.add(f"{horizontal}, {vertical}") #easier formatting for strings

        moves = input_file.readline().strip().split(", ")

        for move in moves:
            if (move[0] == 'R' and direction == "west") or (move[0] == 'L' and direction == "east"):
                direction = "north"
            elif (move[0] == 'R' and direction == "north") or (move[0] == 'L' and direction == "south"):
                direction = "east"
            elif (move[0] == 'R' and direction == "east") or (move[0] == 'L' and direction == "west"):
                direction = "south"
            elif (move[0] == 'R' and direction == "south") or (move[0] == 'L' and direction == "north"):
                direction = "west"

            steps = int(move[1:])
            for _ in range(steps): 
                if direction == "north":
                    vertical += 1
                elif direction == "south":
                    vertical -= 1
                elif direction == "east":
                    horizontal += 1
                elif direction == "west":
                    horizontal -= 1

                #Part 2 Answer
                position = f"{horizontal}, {vertical}"
                if position in locations_visited:
                    print(position)
                    return
                else:
                    locations_visited.add(position)


        print(abs(horizontal) + abs(vertical))

if __name__ == "__main__":
    main()