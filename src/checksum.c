#include "checksum.h"
#include <stdio.h>

int sum(int* array, int size) {
    int total = 0;
    for (int i = 0; i < size; i++) {
      total += array[i];
    }
    return total;
}


unsigned int checksum (
  unsigned char *buf,
  int length)
{

  unsigned int sum32 = 0;
  unsigned int hi, lo, hicarry, locarry, i;

  // Accumulate the sum of the high-order 16 bits and the */
  // low-order 16 bits of each 32-bit word, separately. */
  // The first byte in each pair is the most significant. */
  // This algorithm works on both big and little endian machines.
  hi = (sum32 >> 16);
  lo = sum32 & 0xFFFF;
  for (i=0; i < length; i+=4) {
    hi += ((buf[i] << 8) + buf[i+1]);
    lo += ((buf[i+2] << 8) + buf[i+3]);
  }

  // fold carry bits from each 16 bit sum into the other sum
  hicarry = hi >> 16;
  locarry = lo >> 16;

  while (hicarry || locarry) {
    hi = (hi & 0xFFFF) + locarry;
    lo = (lo & 0xFFFF) + hicarry;
    hicarry = hi >> 16;
    locarry = lo >> 16;
  }

  // Concatenate the high and low parts to form the full 32-bit checksum
  return (hi << 16) + lo;
}
