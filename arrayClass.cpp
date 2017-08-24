
//
// arrayClass . cpp
//
//
// Created by Marie Galligan on 04/06/2015.
//
//

#include <iostream>
#include "arrayClass.h"

using namespace std;

int main() {
  // Declare an array with 10 elements
  // calls the constructor of the class
  IntArray cArray(10); // Passes argument of 10 to the constructor

  // Fill the array with numbers 1 through 10
  for (int i=0; i<10; i++)
    cArray[i] = i+1; // the [] operator is overloaded
    // to allow access to elements of the
    // array data member

  // Resize the array to 8 elements
  cArray.Resize(8); // calling the Resize member function

  // Insert the number 20 before the 5th element
  cArray.InsertBefore(20, 5);

  // Remove the 3rd element
  cArray.Remove(3);

  // Add 30 and 40 to the end and beginning
  cArray.InsertAtEnd(30);
  cArray.InsertAtBeginning(40);

  // Find and print the minimum value of the array
  cArray.minArray();

  // Print out all the numbers
  for (int j=0; j<cArray.GetLength(); j++)
    cout << cArray[j] << endl;

  return 0;
}

