/*
 * gray.c --
 *
 *      More procedures to dither grey-scale images.
 *
 */

/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */



#ifdef ENABLE_GRAY_DITHER


#include "../imageMpegLocal.H"
#include "video.h"
#include "dither.h"


/*
 *--------------------------------------------------------------
 *
 * GrayDitherImage --
 *
 *	Dithers image into 128 gray scales. Simply maps luminance
 *      value into 1 of 128 gray scale colors (divide by two, essentially).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::GrayDitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *out,
    int h, int w)
{

  int i, max = w*h/16;

  for (i=0; i<max; i++) {
    out[0] = pixel[lum[0]];
    out[1] = pixel[lum[1]];
    out[2] = pixel[lum[2]];
    out[3] = pixel[lum[3]];
    out[4] = pixel[lum[4]];
    out[5] = pixel[lum[5]];
    out[6] = pixel[lum[6]];
    out[7] = pixel[lum[7]];
    out[8] = pixel[lum[8]];
    out[9] = pixel[lum[9]];
    out[10] = pixel[lum[10]];
    out[11] = pixel[lum[11]];
    out[12] = pixel[lum[12]];
    out[13] = pixel[lum[13]];
    out[14] = pixel[lum[14]];
    out[15] = pixel[lum[15]];
    out += 16;
    lum += 16;
  }
}


void
imageMpegDecoder::Gray16DitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *p_out,
    int h,int  w)
{

  unsigned short *out = (unsigned short *)p_out;

  int i, max = w*h/16;

  for (i=0; i<max; i++) {
    out[0] = wpixel[lum[0]];
    out[1] = wpixel[lum[1]];
    out[2] = wpixel[lum[2]];
    out[3] = wpixel[lum[3]];
    out[4] = wpixel[lum[4]];
    out[5] = wpixel[lum[5]];
    out[6] = wpixel[lum[6]];
    out[7] = wpixel[lum[7]];
    out[8] = wpixel[lum[8]];
    out[9] = wpixel[lum[9]];
    out[10] = wpixel[lum[10]];
    out[11] = wpixel[lum[11]];
    out[12] = wpixel[lum[12]];
    out[13] = wpixel[lum[13]];
    out[14] = wpixel[lum[14]];
    out[15] = wpixel[lum[15]];
    out += 16;
    lum += 16;
  }
}

void
imageMpegDecoder::Gray32DitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *p_out,
    int h,int  w)
{

  unsigned int *out = (unsigned int *)p_out;
  int i, max = w*h/16;

  for (i=0; i<max; i++) {
    out[0] = wpixel[lum[0]];
    out[1] = wpixel[lum[1]];
    out[2] = wpixel[lum[2]];
    out[3] = wpixel[lum[3]];
    out[4] = wpixel[lum[4]];
    out[5] = wpixel[lum[5]];
    out[6] = wpixel[lum[6]];
    out[7] = wpixel[lum[7]];
    out[8] = wpixel[lum[8]];
    out[9] = wpixel[lum[9]];
    out[10] = wpixel[lum[10]];
    out[11] = wpixel[lum[11]];
    out[12] = wpixel[lum[12]];
    out[13] = wpixel[lum[13]];
    out[14] = wpixel[lum[14]];
    out[15] = wpixel[lum[15]];
    out += 16;
    lum += 16;
  }
}


/*
 *--------------------------------------------------------------
 *
 * Gray2DitherImage --
 *
 *	Dithers image into 128 gray scales. Simply maps luminance
 *      value into 1 of 128 gray scale colors (divide by two, essentially).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::Gray2DitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *p_out,
    int h,int  w)
{

  int x, y;
  unsigned short *out = (unsigned short *)p_out;
  unsigned short *out2 = out + w;

  for (y=0; y<h; y++) {
    for (x=0; x<w; x+=16) {
      out[0] = out2[0] = wpixel[lum[0]];
      out[1] = out2[1] = wpixel[lum[1]];
      out[2] = out2[2] = wpixel[lum[2]];
      out[3] = out2[3] = wpixel[lum[3]];
      out[4] = out2[4] = wpixel[lum[4]];
      out[5] = out2[5] = wpixel[lum[5]];
      out[6] = out2[6] = wpixel[lum[6]];
      out[7] = out2[7] = wpixel[lum[7]];
      out[8] = out2[8] = wpixel[lum[8]];
      out[9] = out2[9] = wpixel[lum[9]];
      out[10] = out2[10] = wpixel[lum[10]];
      out[11] = out2[11] = wpixel[lum[11]];
      out[12] = out2[12] = wpixel[lum[12]];
      out[13] = out2[13] = wpixel[lum[13]];
      out[14] = out2[14] = wpixel[lum[14]];
      out[15] = out2[15] = wpixel[lum[15]];
      out += 16;
      out2 += 16;
      lum += 16;
    }
  out += w;
  out2 += w;
  }
}

void
imageMpegDecoder::Gray216DitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *p_out,
    int h,int  w)
{

  int x, y;
  unsigned int *out = (unsigned int *)p_out;
  unsigned int *out2 = out + w;

  for (y=0; y<h; y++) {
    for (x=0; x<w; x+=16) {
      out[0] = out2[0] = wpixel[lum[0]];
      out[1] = out2[1] = wpixel[lum[1]];
      out[2] = out2[2] = wpixel[lum[2]];
      out[3] = out2[3] = wpixel[lum[3]];
      out[4] = out2[4] = wpixel[lum[4]];
      out[5] = out2[5] = wpixel[lum[5]];
      out[6] = out2[6] = wpixel[lum[6]];
      out[7] = out2[7] = wpixel[lum[7]];
      out[8] = out2[8] = wpixel[lum[8]];
      out[9] = out2[9] = wpixel[lum[9]];
      out[10] = out2[10] = wpixel[lum[10]];
      out[11] = out2[11] = wpixel[lum[11]];
      out[12] = out2[12] = wpixel[lum[12]];
      out[13] = out2[13] = wpixel[lum[13]];
      out[14] = out2[14] = wpixel[lum[14]];
      out[15] = out2[15] = wpixel[lum[15]];
      out += 16;
      out2 += 16;
      lum += 16;
    }
  out += w;
  out2 += w;
  }
}

#ifndef SIXTYFOUR_BIT

void
imageMpegDecoder::Gray232DitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *p_out,
    int h,int  w)
{

  int x, y;
  unsigned int *out = (unsigned int *)p_out;
  unsigned int *out2 = out + w * 2;

  for (y=0; y<h; y++) {
    for (x=0; x<w; x+=16) {
      out[0] = out2[0] = out[1] = out2[1] = wpixel[lum[0]];
      out[2] = out2[2] = out[3] = out2[3] = wpixel[lum[1]];
      out[4] = out2[4] = out[5] = out2[5] = wpixel[lum[2]];
      out[6] = out2[6] = out[7] = out2[7] = wpixel[lum[3]];
      out[8] = out2[8] = out[9] = out2[9] = wpixel[lum[4]];
      out[10] = out2[10] = out[11] = out2[11] = wpixel[lum[5]];
      out[12] = out2[12] = out[13] = out2[13] = wpixel[lum[6]];
      out[14] = out2[14] = out[15] = out2[15] = wpixel[lum[7]];
      out[16] = out2[16] = out[17] = out2[17] = wpixel[lum[8]];
      out[18] = out2[18] = out[19] = out2[19] = wpixel[lum[9]];
      out[20] = out2[20] = out[21] = out2[21] = wpixel[lum[10]];
      out[22] = out2[22] = out[23] = out2[23] = wpixel[lum[11]];
      out[24] = out2[24] = out[25] = out2[25] = wpixel[lum[12]];
      out[26] = out2[26] = out[27] = out2[27] = wpixel[lum[13]];
      out[28] = out2[28] = out[29] = out2[29] = wpixel[lum[14]];
      out[30] = out2[30] = out[31] = out2[31] = wpixel[lum[15]];
      out += 32;
      out2 += 32;
      lum += 16;
    }
  out += w * 2;
  out2 += w * 2;
  }
}

#else /* def SIXTYFOUR_BIT */

void
imageMpegDecoder::Gray232DitherImage(
    unsigned char *lum,
    unsigned char *cr,
    unsigned char *cb,
    unsigned char *p_out,
    int h,int  w)
{

  int x, y;
  unsigned long *out = (unsigned long *)p_out;
  unsigned long *out2 = out + w;

  for (y=0; y<h; y++) {
    for (x=0; x<w; x+=16) {
      out[0] = out2[0]   = wpixel[lum[0]];
      out[2] = out2[2]   = wpixel[lum[1]];
      out[4] = out2[4]   = wpixel[lum[2]];
      out[6] = out2[6]   = wpixel[lum[3]];
      out[8] = out2[8]   = wpixel[lum[4]];
      out[10] = out2[10] = wpixel[lum[5]];
      out[12] = out2[12] = wpixel[lum[6]];
      out[14] = out2[14] = wpixel[lum[7]];
      out[16] = out2[16] = wpixel[lum[8]];
      out[18] = out2[18] = wpixel[lum[9]];
      out[20] = out2[20] = wpixel[lum[10]];
      out[22] = out2[22] = wpixel[lum[11]];
      out[24] = out2[24] = wpixel[lum[12]];
      out[26] = out2[26] = wpixel[lum[13]];
      out[28] = out2[28] = wpixel[lum[14]];
      out[30] = out2[30] = wpixel[lum[15]];
      out += 16;
      out2 += 16;
      lum += 16;
    }
  out += w;
  out2 += w;
  }
}

#endif /* def SIXTYFOUR_BIT */
#endif
