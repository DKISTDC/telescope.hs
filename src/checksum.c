#include "checksum.h"
#include <stdio.h>

// From FITS standard 4.0

int sum(int* array, int size) {
    int total = 0;
    for (int i = 0; i < size; i++) {
      total += array[i];
    }
    return total;
}

// J.5. Example C code for accumulating the checksum
// --------------------------------------------------

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



// J.6. Example C code for ASCII encoding
// --------------------------------------------------

unsigned int exclude[13] = {0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60 };

int offset = 0x30; /* ASCII 0 (zero) */
unsigned long mask[4] = { 0xff000000, 0xff0000, 0xff00, 0xff };

void char_encode (
    unsigned int value, // 1's complement of the checksum value to be encoded
    char *ascii)        // Output 16-character encoded string
{
  int byte, quotient, remainder, ch[4], check, i, j, k; char asc[32];

  for (i=0; i < 4; i++) {
    /* each byte becomes four */
    byte = (value & mask[i]) >> ((3 - i) * 8);
    quotient = byte / 4 + offset;
    remainder = byte % 4;
    for (j=0; j < 4; j++)
      ch[j] = quotient;

    ch[0] += remainder;

    // avoid ASCII punctuation
    for (check=1; check;)
      for (check=0, k=0; k < 13; k++)
        for (j=0; j < 4; j+=2)
          if (ch[j]==exclude[k] || ch[j+1]==exclude[k]) {
            ch[j]++;
            ch[j+1]--;
            check++;
          }

    // assign the bytes
    for (j=0; j < 4; j++)
      asc[4*j+i] = ch[j];
  }

  // Permute the bytes for FITS
  for (i=0; i < 16; i++)
    ascii[i] = asc[(i+15)%16];

  // terminate the string
  ascii[16] = 0;
}
