#include<iostream>
#include<cstdlib>

using namespace std;

// constant for where visited
const int VISITED = 1;
// create a grid 1000 x 1000
const int N = 1000;
// offset constant to prevent negative indexing on board.
const int OFFSET = 500;

int main() {

    int visited = VISITED;
    // create a board size
    int N = 1000;
    // have offset to prevent setting negative indexing
    int OFFSET = 500;
    // probably don't need to track the board but just in case
    int board[N][N];
    //east = 1, west = 2, north =3, south = 4
    int direction[4] = {1,2,3,4};

    // the location to find - must take into account the offset.
    int xLocation = OFFSET - 10;
    int yLocation = OFFSET + 30;
    
    int curX = 0 + OFFSET;
    int curY = 0 + OFFSET;
   
    // keep track of everwhere that was visited in case we want to draw the grid 
    board[curX][curY] = visited;
    
    srand(1234);

    int dir;
    int totalSteps = 0;
    int numSteps = 1000;
    bool found = false;
    // while less than max steps walked - 1000, and haven't found the place then try again
    while (totalSteps < numSteps && !found) {
        dir = rand() %4 +1;
        cout << dir;
        // going east
        if (dir == 1) {
            board[curX][++curY] = visited;
        // going west
        } else if(dir == 2) {
            board[curX][--curY] = visited;
        // going north
        } else if(dir == 3) {
            board[--curX][curY] = visited;
        // going south - make these const.
        } else if(dir == 4) {
            board[++curX][curY] = visited;
        }
        // check to see if we have found the location
        if (curX == xLocation && curY == yLocation) {
            found = true;
        }
        totalSteps++;         
   }
   if (found) {
       cout << "found";
   }
   return found;
}

