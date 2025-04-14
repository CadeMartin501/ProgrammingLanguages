import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;

public class AdventOfCodeDay1 {
    public static void main(String[] args) throws FileNotFoundException {
        Scanner input = new Scanner(new File("Java/movement"));
        String direction = "north";
        int horizontal = 0;
        int vertical = 0;
        Set<String> locationsVisited = new HashSet<>();
        locationsVisited.add(horizontal + ", " + vertical);

        String[] moves = input.nextLine().trim().split(", ");
        for (String move : moves) {
            if ((move.charAt(0) == 'R' && direction.equals("west")) ||
                    (move.charAt(0) == 'L' && direction.equals("east"))) {
                direction = "north";
            } else if ((move.charAt(0) == 'R' && direction.equals("north")) ||
                    (move.charAt(0) == 'L' && direction.equals("south"))) {
                direction = "east";
            } else if ((move.charAt(0) == 'R' && direction.equals("east")) ||
                    (move.charAt(0) == 'L' && direction.equals("west"))) {
                direction = "south";
            } else if ((move.charAt(0) == 'R' && direction.equals("south")) ||
                    (move.charAt(0) == 'L' && direction.equals("north"))) {
                direction = "west";
            }
            int steps = Integer.parseInt(move.substring(1));
            {
                for (int j = 0; j < steps; j++) {
                    switch (direction) {
                        case ("north"):
                            vertical++;
                            break;

                        case ("south"):
                            vertical--;
                            break;

                        case ("east"):
                            horizontal++;
                            break;

                        case ("west"):
                            horizontal--;
                            break;
                    }
                    // Start of Part 2 Answer.
                    String p = horizontal + ", " + vertical;
                    if (locationsVisited.contains(p)) {
                        System.out.println(p);
                        input.close();
                        System.exit(0);
                    } else {
                        locationsVisited.add(horizontal + ", " + vertical);
                    }
                    //End of Part 2 Answer.
                }
            }
        }
        //Part 1 Answer.
        System.out.println(Math.abs(horizontal) + Math.abs(vertical));
        input.close();
    }
}