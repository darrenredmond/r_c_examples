//
// array_class .h
// from website http :// www. learncpp . com/cpp - tutorial /104 - container - classes /
//
// Created by Marie Galligan on 04/06/2015.
//
//

#ifndef ARRAYCLASS_H
#define ARRAYCLASS_H

#include <assert.h> // for assert ()
#include <iostream>


using namespace std;

class IntArray
{
  private :
    int m_nLength ;
    int * m_pnData ;

  public :
    IntArray ()
    {
      m_nLength = 0;
      m_pnData = 0;
    }
  
    IntArray ( int nLength )
    {
      m_pnData = new int [ nLength ];
      m_nLength = nLength ;
    }

    ~ IntArray ()
    {
      delete [] m_pnData ;
    }

    void Erase ()
    {
      delete [] m_pnData ;
      // We need to make sure we set m_pnData to 0 here , otherwise it will
      // be left pointing at deallocated memory !
      m_pnData = 0;
      m_nLength = 0;
    }

    int & operator []( int nIndex )
    {
      assert ( nIndex >= 0 && nIndex < m_nLength );
      return m_pnData [ nIndex ];
    }

    // Reallocate resizes the array . Any existing elements will be destroyed .
    // This function operates quickly .
    void Reallocate (int nNewLength )
    {
      // First we delete any existing elements

      Erase ();

      // If our array is going to be empty now , return here
      if ( nNewLength <= 0)
        return ;

      // Then we have to allocate new elements
      m_pnData = new int [ nNewLength ];
      m_nLength = nNewLength ;
    }

    // Resize resizes the array . Any existing elements will be kept .
    // This function operates slowly .
    void Resize ( int nNewLength )
    {
      // If we are resizing to an empty array , do that and return
      if ( nNewLength <= 0)
      {
        Erase ();
        return ;
      }

      // Now we can assume nNewLength is at least 1 element .
      // This algorithmworks as follows :
      // First we are going to allocate a new array . Then we
      // are going to copy elements from the existing array
      // to the new array . Once that is done ,
      // we can destroy the old array , and make m_pnData
      // point to the new array .

      // First we have to allocate a new array
      int * pnData = new int[ nNewLength ];

      // Then we have to figure out how many elements to
      // copy from the existing array to the new array .
      // We want to copy as many elements as there are
      // in the smaller of the two arrays .
      if ( m_nLength > 0)
  {

  // the ? : operator works like an ifelse statement in R
  int nElementsToCopy =
  ( nNewLength > m_nLength ) ? m_nLength : nNewLength ;

  // Now copy the elements one by one
  for (int nIndex =0; nIndex < nElementsToCopy ; nIndex ++)
  pnData [ nIndex ] = m_pnData [ nIndex ];
  }

  // Now we can delete the old array because we don ’t need it any more
  delete [] m_pnData ;

  // And use the new array instead ! Note that this simply
  // makes m_pnData point to the same address as the
  // new array we dynamically allocated . Because
  // pnData was dynamically allocated , it won ’t be destroyed
  // when it goes out of scope .
  m_pnData = pnData ;
  m_nLength = nNewLength ;
  }


  void InsertBefore ( int nValue , int nIndex )
  {
  // Sanity check our nIndex value
  assert ( nIndex >= 0 && nIndex <= m_nLength );

  // First create a new array one element larger than the old array

  int * pnData = new int[ m_nLength +1];

  // Copy all of the elements up to the index
  for (int nBefore =0; nBefore < nIndex ; nBefore ++)
  pnData [ nBefore ] = m_pnData [ nBefore ];

  // insert our new element into the new array
  pnData [ nIndex ] = nValue ;

  // Copy all of the values after the inserted element
  for (int nAfter = nIndex ; nAfter < m_nLength ; nAfter ++)
  pnData [ nAfter +1] = m_pnData [ nAfter ];

  // Finally , delete the old array , and use the new array instead
  delete [] m_pnData ;
  m_pnData = pnData ;
  m_nLength += 1;
  }

  void Remove ( int nIndex )
  {
  // Sanity check our nIndex value
  assert ( nIndex >= 0 && nIndex < m_nLength );

  // First create a new array one element smaller than the old array
  int * pnData = new int[ m_nLength -1];

  // Copy all of the elements up to the index
  for (int nBefore =0; nBefore < nIndex ; nBefore ++)
  pnData [ nBefore ] = m_pnData [ nBefore ];

  // Copy all of the values after the inserted element
  for (int nAfter = nIndex +1; nAfter < m_nLength ; nAfter ++)
  pnData [ nAfter -1] = m_pnData [ nAfter ];

  // Finally , delete the old array , and use the new array instead
  delete [] m_pnData ;
  m_pnData = pnData ;
  m_nLength -= 1;
  }

  // A couple of additional functions just for convenience
  void InsertAtBeginning ( int nValue ) { InsertBefore ( nValue , 0); }
  void InsertAtEnd ( int nValue ) { InsertBefore ( nValue , m_nLength ); }

  int GetLength () { return m_nLength ; }

  // function to find the minimum value of the array
  void minArray ()
  {
  int minVal = m_pnData [ 0 ];
 
  for ( int j = 0; j < m_nLength ; j ++ ){

  if( m_pnData [ j ] < minVal )
  {
  minVal = m_pnData [ j ];
  }

  }
  cout << " Minimum value of the array is " << minVal << endl ;
  } // end of minArray ()


}; // end of class definition

# endif
// end of header file
