/* 
 * video.c --
 *
 *      This file contains C code that implements the video decoder model.
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

/*
 * Portions of this software Copyright (c) 1995 Brown University.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement
 * is hereby granted, provided that the above copyright notice and the
 * following two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL BROWN UNIVERSITY BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF BROWN
 * UNIVERSITY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * BROWN UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS"
 * BASIS, AND BROWN UNIVERSITY HAS NO OBLIGATION TO PROVIDE MAINTENANCE,
 * SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifndef MIPS
#include <sys/time.h>
#else
#include <sys/types.h>
#include <sys/system.h>
#endif

#include "../imageMpegLocal.H"


#ifndef NOCONTROLS
#include "ctrlbar.h"
#endif
#include "decoders.h"
#include "video.h"
#include "util.h"
#include "table.h"


/*
   Changes to make the code reentrant:
     de-globalized: totNumFrames, realTimeStart, matched_depth, ditherType,
       curBits, ReconPMBlock statics, first, [lc]max[xy], ditherFlags,
       vid_stream, Parse_done, seekValue, ReadPack static, sys_layer,
       bitOffset, bitLength, bitBuffer, curVidStream,
     X globals to xinfo (window, et al)
     use vid_stream->film_has_ended instead of FilmState
     lookup tables only initialized once, global as possible
        (default_intra_matrix, zigzag, zigzag_direct, scan)
     get rid of setjmp, long jmp
   Additional changes:
     if DISABLE_DITHER defined then do not compile dithering code
   -lsh@cs.brown.edu (Loring Holden)
 */

/* Macro for returning 1 if num is positive, -1 if negative, 0 if 0. */

#define Sign(num) ((num > 0) ? 1 : ((num == 0) ? 0 : -1))

/* Set up array for fast conversion from zig zag order to row/column
   coordinates.
*/

unsigned char default_intra_matrix[64] = {
    8, 16, 19, 22, 26, 27, 29, 34,
    16, 16, 22, 24, 27, 29, 34, 37,
    19, 22, 26, 27, 29, 34, 34, 38,
    22, 22, 26, 27, 29, 34, 37, 40,
    22, 26, 27, 29, 32, 35, 40, 48,
    26, 27, 29, 32, 35, 40, 48, 58,
    26, 27, 29, 34, 38, 46, 56, 69,
  27, 29, 35, 38, 46, 56, 69, 83};


int zigzag[64][2] = {
  0, 0, 1, 0, 0, 1, 0, 2, 1, 1, 2, 0, 3, 0, 2, 1, 1, 2, 0, 3, 0, 4, 1, 3,
  2, 2, 3, 1, 4, 0, 5, 0, 4, 1, 3, 2, 2, 3, 1, 4, 0, 5, 0, 6, 1, 5, 2, 4,
  3, 3, 4, 2, 5, 1, 6, 0, 7, 0, 6, 1, 5, 2, 4, 3, 3, 4, 2, 5, 1, 6, 0, 7,
  1, 7, 2, 6, 3, 5, 4, 4, 5, 3, 6, 2, 7, 1, 7, 2, 6, 3, 5, 4, 4, 5, 3, 6,
  2, 7, 3, 7, 4, 6, 5, 5, 6, 4, 7, 3, 7, 4, 6, 5, 5, 6, 4, 7, 5, 7, 6, 6,
  7, 5, 7, 6, 6, 7, 7, 7};
/* Array mapping zigzag to array pointer offset. */

int zigzag_direct[64] = {
  0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12,
  19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35,
  42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63};
/* Set up array for fast conversion from row/column coordinates to
   zig zag order.
*/

const int scan[8][8] = {
  {0, 1, 5, 6, 14, 15, 27, 28},
  {2, 4, 7, 13, 16, 26, 29, 42},
  {3, 8, 12, 17, 25, 30, 41, 43},
  {9, 11, 18, 24, 31, 40, 44, 53},
  {10, 19, 23, 32, 39, 45, 52, 54},
  {20, 22, 33, 38, 46, 51, 55, 60},
  {21, 34, 37, 47, 50, 56, 59, 61},
{35, 36, 48, 49, 57, 58, 62, 63}};






/*
  The following accounts for time and size  spent in various parsing acitivites
  if ANALYSIS has been defined.
*/

#ifdef ANALYSIS



void
imageMpegDecoder::init_stat_struct( Statval *astat)
{
  int j;

  astat->frametype = 0;
  astat->totsize = 0;
  astat->number = 0;
  astat->i_mbsize = 0;
  astat->p_mbsize = 0;
  astat->b_mbsize = 0;
  astat->bi_mbsize = 0;
  astat->i_mbnum = 0;
  astat->p_mbnum = 0;
  astat->b_mbnum = 0;
  astat->bi_mbnum = 0;

  for (j = 0; j < 64; j++) {

    astat->i_mbcbp[j] = 0;
    astat->p_mbcbp[j] = 0;
    astat->b_mbcbp[j] = 0;
    astat->bi_mbcbp[j] = 0;
    astat->i_mbcoeff[j] = 0;
    astat->p_mbcoeff[j] = 0;
    astat->b_mbcoeff[j] = 0;
    astat->bi_mbcoeff[j] = 0;
  }
  astat->tottime = 0.0;
}

void
imageMpegDecoder::init_stats()
{
  int i, j;

  for (i = 0; i < 4; i++) {
    init_stat_struct(&(stat_a[i]));
    stat_a[i].frametype = i;
  }

  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++) {
      cacheHit[i][j] = 0;
      cacheMiss[i][j] = 0;
    }
  }

  bitCount = 0;
}

void
imageMpegDecoder::PrintOneStat()
{
  int i;

  printf("\n");
  switch (stat_a[0].frametype) {
  case I_TYPE:
    printf("I FRAME\n");
    break;
  case P_TYPE:
    printf("P FRAME\n");
    break;
  case B_TYPE:
    printf("B FRAME\n");
    break;
  }

  printf("Size: %d bytes + %d bits\n", stat_a[0].totsize / 8, stat_a[0].totsize % 8);
  if (stat_a[0].i_mbnum > 0) {
    printf("\tI Macro Block Stats:\n");
    printf("\t%d I Macroblocks\n", stat_a[0].i_mbnum);
    printf("\tAvg. Size: %d bytes + %d bits\n",
	   stat_a[0].i_mbsize / (8 * stat_a[0].i_mbnum),
	   (stat_a[0].i_mbsize * stat_a[0].i_mbnum) % 8);
    printf("\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].i_mbcbp[i],
	     stat_a[0].i_mbcbp[i + 1], stat_a[0].i_mbcbp[i + 2], stat_a[0].i_mbcbp[i + 3],
	     stat_a[0].i_mbcbp[i + 4], stat_a[0].i_mbcbp[i + 5], stat_a[0].i_mbcbp[i + 6],
	     stat_a[0].i_mbcbp[i + 7]);
    }
    printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].i_mbcoeff[i],
	     stat_a[0].i_mbcoeff[i + 1], stat_a[0].i_mbcoeff[i + 2],
	     stat_a[0].i_mbcoeff[i + 3], stat_a[0].i_mbcoeff[i + 4],
	     stat_a[0].i_mbcoeff[i + 5], stat_a[0].i_mbcoeff[i + 6],
	     stat_a[0].i_mbcoeff[i + 7]);
    }
  }
  if (stat_a[0].p_mbnum > 0) {
    printf("\tP Macro Block Stats:\n");
    printf("\t%d P Macroblocks\n", stat_a[0].p_mbnum);
    printf("\tAvg. Size: %d bytes + %d bits\n",
	   stat_a[0].p_mbsize / (8 * stat_a[0].p_mbnum),
	   (stat_a[0].p_mbsize / stat_a[0].p_mbnum) % 8);
    printf("\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].p_mbcbp[i],
	     stat_a[0].p_mbcbp[i + 1], stat_a[0].p_mbcbp[i + 2], stat_a[0].p_mbcbp[i + 3],
	     stat_a[0].p_mbcbp[i + 4], stat_a[0].p_mbcbp[i + 5], stat_a[0].p_mbcbp[i + 6],
	     stat_a[0].p_mbcbp[i + 7]);
    }
    printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].p_mbcoeff[i],
	     stat_a[0].p_mbcoeff[i + 1], stat_a[0].p_mbcoeff[i + 2],
	     stat_a[0].p_mbcoeff[i + 3], stat_a[0].p_mbcoeff[i + 4],
	     stat_a[0].p_mbcoeff[i + 5], stat_a[0].p_mbcoeff[i + 6],
	     stat_a[0].p_mbcoeff[i + 7]);
    }
  }
  if (stat_a[0].b_mbnum > 0) {
    printf("\tB Macro Block Stats:\n");
    printf("\t%d B Macroblocks\n", stat_a[0].b_mbnum);
    printf("\tAvg. Size: %d bytes + %d bits\n",
	   stat_a[0].b_mbsize / (8 * stat_a[0].b_mbnum),
	   (stat_a[0].b_mbsize / stat_a[0].b_mbnum) % 8);
    printf("\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].b_mbcbp[i],
	     stat_a[0].b_mbcbp[i + 1], stat_a[0].b_mbcbp[i + 2], stat_a[0].b_mbcbp[i + 3],
	     stat_a[0].b_mbcbp[i + 4], stat_a[0].b_mbcbp[i + 5], stat_a[0].b_mbcbp[i + 6],
	     stat_a[0].b_mbcbp[i + 7]);
    }
    printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].b_mbcoeff[i],
	     stat_a[0].b_mbcoeff[i + 1], stat_a[0].b_mbcoeff[i + 2],
	     stat_a[0].b_mbcoeff[i + 3], stat_a[0].b_mbcoeff[i + 4],
	     stat_a[0].b_mbcoeff[i + 5], stat_a[0].b_mbcoeff[i + 6],
	     stat_a[0].b_mbcoeff[i + 7]);
    }
  }
  if (stat_a[0].bi_mbnum > 0) {
    printf("\tBi Macro Block Stats:\n");
    printf("\t%d Bi Macroblocks\n", stat_a[0].bi_mbnum);
    printf("\tAvg. Size: %d bytes + %d bits\n",
	   stat_a[0].bi_mbsize / (8 * stat_a[0].bi_mbnum),
	   (stat_a[0].bi_mbsize * stat_a[0].bi_mbnum) % 8);
    printf("\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].bi_mbcbp[i],
	     stat_a[0].bi_mbcbp[i + 1], stat_a[0].bi_mbcbp[i + 2], stat_a[0].bi_mbcbp[i + 3],
	     stat_a[0].bi_mbcbp[i + 4], stat_a[0].bi_mbcbp[i + 5], stat_a[0].bi_mbcbp[i + 6],
	     stat_a[0].bi_mbcbp[i + 7]);
    }
    printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[0].bi_mbcoeff[i],
	     stat_a[0].bi_mbcoeff[i + 1], stat_a[0].bi_mbcoeff[i + 2],
	     stat_a[0].bi_mbcoeff[i + 3], stat_a[0].bi_mbcoeff[i + 4],
	     stat_a[0].bi_mbcoeff[i + 5], stat_a[0].bi_mbcoeff[i + 6],
	     stat_a[0].bi_mbcoeff[i + 7]);
    }
  }
  printf("\nTime to Decode: %g secs.\n", stat_a[0].tottime);
  printf("****************\n");
}

void
imageMpegDecoder::PrintAllStats( VidStream *vid_stream)
{
  int i, j;
  unsigned int supertot, supernum;
  double supertime;

  printf("\n");
  printf("General Info: \n");
  printf("Width: %d\nHeight: %d\n", vid_stream->mb_width * 16, vid_stream->mb_height * 16);

  for (i = 1; i < 4; i++) {

    if (stat_a[i].number == 0)
      continue;

    switch (i) {
    case 1:
      printf("I FRAMES\n");
      break;
    case 2:
      printf("P FRAMES\n");
      break;
    case 3:
      printf("B FRAMES\n");
      break;
    }

    printf("Number: %d\n", stat_a[i].number);
    printf("Avg. Size: %d bytes + %d bits\n",
	   stat_a[i].totsize / (8 * stat_a[i].number), (stat_a[i].totsize / stat_a[i].number) % 8);
    if (stat_a[i].i_mbnum > 0) {
      printf("\tI Macro Block Stats:\n");
      printf("\t%d I Macroblocks\n", stat_a[i].i_mbnum);
      printf("\tAvg. Size: %d bytes + %d bits\n",
	     stat_a[i].i_mbsize / (8 * stat_a[i].i_mbnum),
	     (stat_a[i].i_mbsize / stat_a[i].i_mbnum) % 8);
      printf("\t\tCoded Block Pattern Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].i_mbcbp[j],
	       stat_a[i].i_mbcbp[j + 1], stat_a[i].i_mbcbp[j + 2], stat_a[i].i_mbcbp[j + 3],
	       stat_a[i].i_mbcbp[j + 4], stat_a[i].i_mbcbp[j + 5], stat_a[i].i_mbcbp[j + 6],
	       stat_a[i].i_mbcbp[j + 7]);
      }
      printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].i_mbcoeff[j],
	       stat_a[i].i_mbcoeff[j + 1], stat_a[i].i_mbcoeff[j + 2],
	       stat_a[i].i_mbcoeff[j + 3], stat_a[i].i_mbcoeff[j + 4],
	       stat_a[i].i_mbcoeff[j + 5], stat_a[i].i_mbcoeff[j + 6],
	       stat_a[i].i_mbcoeff[j + 7]);
      }
    }
    if (stat_a[i].p_mbnum > 0) {
      printf("\tP Macro Block Stats:\n");
      printf("\t%d P Macroblocks\n", stat_a[i].p_mbnum);
      printf("\tAvg. Size: %d bytes + %d bits\n",
	     stat_a[i].p_mbsize / (8 * stat_a[i].p_mbnum),
	     (stat_a[i].p_mbsize / stat_a[i].p_mbnum) % 8);
      printf("\t\tCoded Block Pattern Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].p_mbcbp[j],
	       stat_a[i].p_mbcbp[j + 1], stat_a[i].p_mbcbp[j + 2], stat_a[i].p_mbcbp[j + 3],
	       stat_a[i].p_mbcbp[j + 4], stat_a[i].p_mbcbp[j + 5], stat_a[i].p_mbcbp[j + 6],
	       stat_a[i].p_mbcbp[j + 7]);
      }
      printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].p_mbcoeff[j],
	       stat_a[i].p_mbcoeff[j + 1], stat_a[i].p_mbcoeff[j + 2],
	       stat_a[i].p_mbcoeff[j + 3], stat_a[i].p_mbcoeff[j + 4],
	       stat_a[i].p_mbcoeff[j + 5], stat_a[i].p_mbcoeff[j + 6],
	       stat_a[i].p_mbcoeff[j + 7]);
      }
    }
    if (stat_a[i].b_mbnum > 0) {
      printf("\tB Macro Block Stats:\n");
      printf("\t%d B Macroblocks\n", stat_a[i].b_mbnum);
      printf("\tAvg. Size: %d bytes + %d bits\n",
	     stat_a[i].b_mbsize / (8 * stat_a[i].b_mbnum),
	     (stat_a[i].b_mbsize * stat_a[i].b_mbnum) % 8);
      printf("\t\tCoded Block Pattern Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].b_mbcbp[j],
	       stat_a[i].b_mbcbp[j + 1], stat_a[i].b_mbcbp[j + 2], stat_a[i].b_mbcbp[j + 3],
	       stat_a[i].b_mbcbp[j + 4], stat_a[i].b_mbcbp[j + 5], stat_a[i].b_mbcbp[j + 6],
	       stat_a[i].b_mbcbp[j + 7]);
      }
      printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].b_mbcoeff[j],
	       stat_a[i].b_mbcoeff[j + 1], stat_a[i].b_mbcoeff[j + 2],
	       stat_a[i].b_mbcoeff[j + 3], stat_a[i].b_mbcoeff[j + 4],
	       stat_a[i].b_mbcoeff[j + 5], stat_a[i].b_mbcoeff[j + 6],
	       stat_a[i].b_mbcoeff[j + 7]);
      }
    }
    if (stat_a[i].bi_mbnum > 0) {
      printf("\tBi Macro Block Stats:\n");
      printf("\t%d Bi Macroblocks\n", stat_a[i].bi_mbnum);
      printf("\tAvg. Size: %d bytes + %d bits\n",
	     stat_a[i].bi_mbsize / (8 * stat_a[i].bi_mbnum),
	     (stat_a[i].bi_mbsize * stat_a[i].bi_mbnum) % 8);
      printf("\t\tCoded Block Pattern Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].bi_mbcbp[j],
	       stat_a[i].bi_mbcbp[j + 1], stat_a[i].bi_mbcbp[j + 2], stat_a[i].bi_mbcbp[j + 3],
	       stat_a[i].bi_mbcbp[j + 4], stat_a[i].bi_mbcbp[j + 5], stat_a[i].bi_mbcbp[j + 6],
	       stat_a[i].bi_mbcbp[j + 7]);
      }
      printf("\n\t\tNumber of Coefficients/Block Histogram:\n");
      for (j = 0; j < 64; j += 8) {
	printf("\t%.6d %.6d %.6d %.6d %.6d %.6d %.6d %.6d\n", stat_a[i].bi_mbcoeff[j],
	       stat_a[i].bi_mbcoeff[j + 1], stat_a[i].bi_mbcoeff[j + 2],
	       stat_a[i].bi_mbcoeff[j + 3], stat_a[i].bi_mbcoeff[j + 4],
	       stat_a[i].bi_mbcoeff[j + 5], stat_a[i].bi_mbcoeff[j + 6],
	       stat_a[i].bi_mbcoeff[j + 7]);
      }
    }
    printf("\nAvg. Time to Decode: %f secs.\n",
	   (stat_a[i].tottime / ((double) stat_a[i].number)));
    printf("\n");
    printf("*************************\n\n");
  }

  supertot = stat_a[1].totsize + stat_a[2].totsize + stat_a[3].totsize;
  supernum = stat_a[1].number + stat_a[2].number + stat_a[3].number;
  supertime = stat_a[1].tottime + stat_a[2].tottime + stat_a[3].tottime;

  printf("Total Number of Frames: %d\n", supernum);
  printf("Avg Frame Size: %d bytes %d bits\n",
	 supertot / (8 * supernum), (supertot / supernum) % 8);
  printf("Total Time Decoding: %g secs.\n", supertime);
  printf("Avg Decoding Time/Frame: %g secs.\n", supertime / ((double) supernum));
  printf("Avg Decoding Frames/Sec: %g secs.\n", ((double) supernum) / supertime);
  printf("\n");

  printf("Cache Hits/Miss\n");
  for (i = 0; i < 8; i++) {
    printf("%.6d/%.6d\t%.6d/%.6d\t%.6d/%.6d\t%.6d/%.6d\n",
	   cacheHit[i][0], cacheMiss[i][0], cacheHit[i][1], cacheMiss[i][1],
	   cacheHit[i][2], cacheMiss[i][2], cacheHit[i][3], cacheMiss[i][3]);
    printf("%.6d/%.6d\t%.6d/%.6d\t%.6d/%.6d\t%.6d/%.6d\n",
	   cacheHit[i][4], cacheMiss[i][4], cacheHit[i][5], cacheMiss[i][5],
	   cacheHit[i][6], cacheMiss[i][6], cacheHit[i][7], cacheMiss[i][7]);
  }

}

void
imageMpegDecoder::CollectStats()
{
  int i, j;

  i = stat_a[0].frametype;

  stat_a[i].totsize += stat_a[0].totsize;
  stat_a[i].number += stat_a[0].number;
  stat_a[i].i_mbsize += stat_a[0].i_mbsize;
  stat_a[i].p_mbsize += stat_a[0].p_mbsize;
  stat_a[i].b_mbsize += stat_a[0].b_mbsize;
  stat_a[i].bi_mbsize += stat_a[0].bi_mbsize;
  stat_a[i].i_mbnum += stat_a[0].i_mbnum;
  stat_a[i].p_mbnum += stat_a[0].p_mbnum;
  stat_a[i].b_mbnum += stat_a[0].b_mbnum;
  stat_a[i].bi_mbnum += stat_a[0].bi_mbnum;

  for (j = 0; j < 64; j++) {

    stat_a[i].i_mbcbp[j] += stat_a[0].i_mbcbp[j];
    stat_a[i].p_mbcbp[j] += stat_a[0].p_mbcbp[j];
    stat_a[i].b_mbcbp[j] += stat_a[0].b_mbcbp[j];
    stat_a[i].bi_mbcbp[j] += stat_a[0].bi_mbcbp[j];
    stat_a[i].i_mbcoeff[j] += stat_a[0].i_mbcoeff[j];
    stat_a[i].p_mbcoeff[j] += stat_a[0].p_mbcoeff[j];
    stat_a[i].b_mbcoeff[j] += stat_a[0].b_mbcoeff[j];
    stat_a[i].bi_mbcoeff[j] += stat_a[0].bi_mbcoeff[j];
  }

  stat_a[i].tottime += stat_a[0].tottime;

  init_stat_struct(&(stat_a[0]));
}


void
imageMpegDecoder::StartTime()
{
  stat_a[0].tottime = ReadSysClock();
}

void
imageMpegDecoder::EndTime()
{
  stat_a[0].tottime = ReadSysClock() - stat_a[0].tottime;
}
#endif


/*
 *--------------------------------------------------------------
 *
 * ReadSysClock --
 *
 *	Computes the current time according to the system clock.
 *
 * Results:
 *  The current time according to the system clock.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

double
imageMpegDecoder::ReadSysClock()
{
  struct timeval tv;
  (void) gettimeofday(&tv, (struct timezone *)NULL);
  return (tv.tv_sec + tv.tv_usec / 1000000.0);
}


/*
 *--------------------------------------------------------------
 *
 * PrintTimeInfo --
 *
 *	Displays statistics regarding the video playback time.
 *
 * Results:
 *  Outputs time statistics to the screen.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::PrintTimeInfo( VidStream *vid_stream)
{
  double spent;

#ifndef NOCONTROLS
  if (ControlShow != CTRLBAR_NONE)
    spent = StopWatch(STOPWATCH_STOP);
  else  
    spent = ReadSysClock() - vid_stream->realTimeStart;
#else
  spent = ReadSysClock() - vid_stream->realTimeStart;
#endif
  if (!quietFlag) {
    printf("\nReal Time Spent (After Initializations): %f secs.\n", spent);
    printf("Number of frames : %d\n",vid_stream->I_number+vid_stream->B_number+vid_stream->P_number);
    printf("Number of I frames : %d\n",vid_stream->I_number);
    printf("Number of B frames : %d\n",vid_stream->B_number);
    printf("Number of P frames : %d\n",vid_stream->P_number);
    printf("Number of display  discard : %d\n",vid_stream->D_discard);
    printf("Number of I frames discard : %d\n",vid_stream->I_discard);
    printf("Number of B frames discard : %d\n",vid_stream->B_discard);
    printf("Number of P frames discard : %d\n",vid_stream->P_discard);
    printf("Avg. Frames/Sec: %f\n",
	   ((double) (vid_stream->I_number+vid_stream->B_number+vid_stream->P_number) / spent));


  }
}


/*
 *--------------------------------------------------------------
 *
 * InitCrop --
 *
 *      Initializes cropTbl - this was taken from newVidStream so
 *      that it wasn't done for each new video stream
 *
 * Results:
 *	None
 *
 * Side effects:
 *      cropTbl will be initialized
 *
 *--------------------------------------------------------------
 */
void
imageMpegDecoder::InitCrop()
{
  int i;

  /* Initialize crop table. */

  for (i = (-MAX_NEG_CROP); i < NUM_CROP_ENTRIES - MAX_NEG_CROP; i++) {
    if (i <= 0) {
      cropTbl[i + MAX_NEG_CROP] = 0;
#ifdef TWELVE_BITS
	} else if (i >= 2047) {
      cropTbl[i + MAX_NEG_CROP] = 2047;
#endif
    } else if (i >= 255) {
      cropTbl[i + MAX_NEG_CROP] = 255;
    } else {
      cropTbl[i + MAX_NEG_CROP] = i;
    }
  }

}



/*
 *--------------------------------------------------------------
 *
 * NewVidStream --
 *
 *	Allocates and initializes a VidStream structure. Takes
 *      as parameter requested size for buffer length.
 *
 * Results:
 *	A pointer to the new VidStream structure.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

VidStream *
imageMpegDecoder::NewVidStream( unsigned int buffer_len)
{
  int i;
  int j;
  VidStream *nouv;

  /* Check for legal buffer length. */

  if (buffer_len < 4)
    return NULL;

  /* Make buffer length multiple of 4. */

  buffer_len = (buffer_len + 3) >> 2;

  /* Allocate memory for new structure. */

  nouv = (VidStream *) malloc(sizeof(VidStream));

  /* Initialize pointers to extension and user data. */

  nouv->group.ext_data = nouv->group.user_data =
    nouv->picture.extra_info = nouv->picture.user_data =
    nouv->picture.ext_data = nouv->slice.extra_info =
    nouv->ext_data = nouv->user_data = NULL;

  /* Copy default intra matrix. */

  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++) {
      nouv->intra_quant_matrix[i][j] = default_intra_matrix[i * 8 + j];
    }
  }

  /* Initialize non intra quantization matrix. */

  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++) {
      nouv->non_intra_quant_matrix[i][j] = 16;
    }
  }

  /* Initialize pointers to image spaces. */

  nouv->current = nouv->past = nouv->future = NULL;
  for (i = 0; i < RING_BUF_SIZE; i++) {
    nouv->ring[i] = NULL;
  }

  /* Create buffer. */
  
  nouv->buf_start = (unsigned int *) malloc(buffer_len * 4);

  /*
   * Set max_buf_length to one less than actual length to deal with messy
   * data without proper seq. end codes.
   */

  nouv->max_buf_length = buffer_len - 1;

  /* Initialize bitstream i/o fields. */

  nouv->bit_offset = 0;
  nouv->buf_length = 0;
  nouv->buffer = nouv->buf_start;

  /* Initialize fields that used to be global */
  nouv->film_has_ended = FALSE;
  nouv->filename = NULL;
  nouv->totNumFrames = 0;
  nouv->ditherFlags = NULL;
  nouv->EOF_flag = FALSE;

  /* Return structure. */

  return nouv;
}



/*
 *--------------------------------------------------------------
 *
 * ResetVidStream --
 *
 *	Re-initializes a VidStream structure. Takes
 *      as parameter a pointer to the VidStream to reset.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ResetVidStream( VidStream *vid)
{
  int i;

  /* Initialize pointers to image spaces. */
  vid->current = vid->past = vid->future = NULL;

  /* Initialize rings */
  for (i = 0; i < RING_BUF_SIZE; i++)
    vid->ring[i]->locked = 0;  /* Unlock */

  /* Initialize bitstream i/o fields. */
  vid->bit_offset = vid->buf_length = 0;
  vid->buffer = vid->buf_start;
  vid->curBits = 0;

  /* We are at the beginning of the film, so film has not ended */
  vid->film_has_ended = FALSE;

  /* Initialize start time */
  vid->realTimeStart = ReadSysClock();

  /* Reset number of frames to zero */
  vid->totNumFrames=0;

  /* Reset EOF_flag to 0 */
  vid->EOF_flag = 0;
}


/*
 *--------------------------------------------------------------
 *
 * DestroyVidStream --
 *
 *	Deallocates a VidStream structure.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
imageMpegDecoder::DestroyVidStream( VidStream *astream, XInfo *xinfo)
{
  int i;

  if (astream->ext_data != NULL)
    free(astream->ext_data);

  if (astream->user_data != NULL)
    free(astream->user_data);

  if (astream->group.ext_data != NULL)
    free(astream->group.ext_data);

  if (astream->group.user_data != NULL)
    free(astream->group.user_data);

  if (astream->picture.extra_info != NULL)
    free(astream->picture.extra_info);

  if (astream->picture.ext_data != NULL)
    free(astream->picture.ext_data);

  if (astream->picture.user_data != NULL)
    free(astream->picture.user_data);

  if (astream->slice.extra_info != NULL)
    free(astream->slice.extra_info);

  if (astream->buf_start != NULL)
    free(astream->buf_start);

  for (i = 0; i < RING_BUF_SIZE; i++) {
    if (astream->ring[i] != NULL) {
      DestroyPictImage(astream->ring[i], xinfo);
      astream->ring[i] = NULL;
    }
  }

  if (astream->ditherFlags !=NULL)
    free(astream->ditherFlags);

  free((char *) astream);
}




/*
 *--------------------------------------------------------------
 *
 * NewPictImage --
 *
 *	Allocates and initializes a PictImage structure.
 *      The width and height of the image space are passed in
 *      as parameters.
 *
 * Results:
 *	A pointer to the new PictImage structure.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

PictImage *
imageMpegDecoder::NewPictImage( VidStream *vid_stream, XInfo *xinfo)
{
  PictImage *nouv;
  int ditherType=vid_stream->ditherType;
  unsigned int width=vid_stream->mb_width * 16;
  unsigned int height=vid_stream->mb_height * 16;
#if SH_MEM
  Display *display=xinfo->display;
#endif

  /* Allocate memory space for new structure. */

  nouv = (PictImage *) malloc(sizeof(PictImage));


  /* Allocate memory for image spaces. */

#ifdef SH_MEM
  nouv->ximage = NULL;
  
  if (shmemFlag) {
    Visual *fc_visual;
    int depth;
    /*
     * factor is 1 or 2
     */
    int factor = 1 + IS_2x2_DITHER(ditherType);

    
#ifndef DISABLE_DITHER
    if (ditherType == FULL_COLOR_DITHER || ditherType == FULL_COLOR2_DITHER) {
#endif
      fc_visual = FindFullColorVisual(display, &depth);
      nouv->ximage = XShmCreateImage(display, fc_visual, (unsigned int) depth,
				    ZPixmap,
				    NULL, &(nouv->shminfo), width * factor,
				    height * factor);
#ifndef DISABLE_DITHER
    } else if (ditherType == FULL_COLOR2_DITHER) {
      fc_visual = FindFullColorVisual(display, &depth);
      nouv->ximage = XShmCreateImage(display, fc_visual, (unsigned int) depth,
				   ZPixmap,
				    NULL, &(nouv->shminfo), width*2, height*2);
    } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
      nouv->ximage = XShmCreateImage(display, None, 1, XYBitmap,
				    NULL, &(nouv->shminfo), width * factor,
				    height * factor);
    } else {
      nouv->ximage = XShmCreateImage(display, None, xinfo->depth, ZPixmap, NULL,
				    &(nouv->shminfo), width * factor,
				    height * factor);
    }
#endif

    /* If no go, then revert to normal Xlib calls. */
    
    if (nouv->ximage == NULL) {
      shmemFlag = 0;
      if (!quietFlag) {
        fprintf(stderr, "Shared memory error, disabling.\n");
        fprintf(stderr, "Ximage error.\n");
      }
      goto shmemerror;
    }

    /* Success here, continue. */

#ifdef LITTLE_ENDIAN_ARCHITECTURE
    nouv->ximage->byte_order = LSBFirst;
    nouv->ximage->bitmap_bit_order = LSBFirst;
#else
    nouv->ximage->byte_order = MSBFirst;
    nouv->ximage->bitmap_bit_order = MSBFirst;
#endif
    
    nouv->shminfo.shmid = shmget(IPC_PRIVATE, (unsigned int) (nouv->ximage->bytes_per_line *
					      nouv->ximage->height),
				IPC_CREAT | 0777);
    
    if (nouv->shminfo.shmid < 0) {
      XDestroyImage(nouv->ximage);
      nouv->ximage = NULL;
      shmemFlag = 0;
      if (!quietFlag) {
        fprintf(stderr, "Shared memory error, disabling.\n");
        fprintf(stderr, "Seg. id. error.\n");
      }
      goto shmemerror;
    }

    nouv->shminfo.shmaddr = (char *) shmat(nouv->shminfo.shmid, 0, 0);

    if (nouv->shminfo.shmaddr == ((char *) -1)) {
      XDestroyImage(nouv->ximage);
      nouv->ximage = NULL;
      shmemFlag = 0;
      if (!quietFlag) {
        fprintf(stderr, "Shared memory error, disabling.\n");
        fprintf(stderr, "Address error.\n");
      }
      goto shmemerror;
    }

    nouv->ximage->data = nouv->shminfo.shmaddr;
    nouv->display = (unsigned char *) nouv->ximage->data;
    nouv->shminfo.readOnly = False;

    XShmAttach(display, &(nouv->shminfo));
    XSync(display, False);

    if (gXErrorFlag) {
      /* Ultimate failure here. */
      XShmDetach(display,&(nouv->shminfo));
      XSync(display, False);
      XDestroyImage(nouv->ximage);
      shmdt (nouv->shminfo.shmaddr);
      shmctl (nouv->shminfo.shmid, IPC_RMID, 0);
      nouv->ximage = NULL;
      shmemFlag = 0;
      if (!quietFlag) {
        fprintf(stderr, "Shared memory error, disabling.\n");
      }
      gXErrorFlag = 0;
      goto shmemerror;
    } else {
      shmctl(nouv->shminfo.shmid, IPC_RMID, 0);
    }
    
    if (!quietFlag) {
      fprintf(stderr, "Sharing memory.\n");
    }
  } else
#endif
    {
    int temp_sz;
    int factor;
shmemerror:
#ifndef DISABLE_DITHER
    temp_sz = vid_stream->matched_depth >> 3;
#else
    temp_sz = 4;
#endif
    if(!temp_sz) temp_sz = 1;
    if(temp_sz == 3) temp_sz = 4;
    factor = 1 + IS_2x2_DITHER(ditherType); /* 1 or 2 */

    nouv->display = (unsigned char *) malloc(width * height * temp_sz *
                                                            factor * factor);
    }
  nouv->luminance = (unsigned char *) malloc(width * height);
  nouv->Cr = (unsigned char *) malloc(width * height / 4);
  nouv->Cb = (unsigned char *) malloc(width * height / 4);
  
  /* Reset locked flag. */
  
  nouv->locked = 0;

  /* Return pointer to new structure. */

  return nouv;
}



/*
 *--------------------------------------------------------------
 *
 * DestroyPictImage --
 *
 *	Deallocates a PictImage structure.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
imageMpegDecoder::DestroyPictImage( PictImage *apictimage, XInfo *xinfo)
{
  if (apictimage->luminance != NULL) {
    free(apictimage->luminance);
  }
  if (apictimage->Cr != NULL) {
    free(apictimage->Cr);
  }
  if (apictimage->Cb != NULL) {
    free(apictimage->Cb);
  }
#ifdef SH_MEM
  if ((apictimage->ximage != NULL)&&!noDisplayFlag&&shmemFlag) {
    if (xinfo!=NULL) XShmDetach(xinfo->display, &(apictimage->shminfo));
    XDestroyImage(apictimage->ximage);
    shmdt(apictimage->shminfo.shmaddr);
    apictimage->ximage = NULL;
    apictimage->display = NULL;
  }
#endif

  if (apictimage->display != NULL) {
    free(apictimage->display);
  }
  free(apictimage);
}


/*
 *--------------------------------------------------------------
 *
 * mpegVidRsrc --
 *
 *      Parses bit stream until MB_QUANTUM number of
 *      macroblocks have been decoded or current slice or
 *      picture ends, whichever comes first. If the start
 *      of a frame is encountered, the frame is time stamped
 *      with the value passed in time_stamp. If the value
 *      passed in buffer is not null, the video stream buffer
 *      is set to buffer and the length of the buffer is
 *      expected in value passed in through length. The current
 *      video stream is set to vid_stream. If vid_stream
 *      is passed as NULL, a new VidStream structure is created
 *      and initialized and used as the current video stream.
 *
 * Results:
 *      A pointer to the video stream structure used.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed. If a picture is completed,
 *      a function is called to display the frame at the correct time.
 *
 *--------------------------------------------------------------
 */

VidStream *
imageMpegDecoder::mpegVidRsrc(
  TimeStamp  time_stamp,
  VidStream *vid_stream,
  int        first,
  XInfo     *xinfo)
{
  unsigned int data;
  int i, status;

  /* If vid_stream is null, create new VidStream structure. */

  if (vid_stream == NULL) {
    return NULL;
  }

  /*
   * If called for the first time, find start code, make sure it is a
   * sequence start code.
   */

  if (first) {
    vid_stream->sys_layer=-1;
    vid_stream->num_left=0;
    vid_stream->leftover_bytes=0;
    vid_stream->seekValue=0;
    vid_stream->Parse_done=FALSE;
    next_start_code(vid_stream);  /* sets curBits */
    show_bits32(data);
    if (data != SEQ_START_CODE) {
      fprintf(stderr, "This is not an MPEG video stream. (%x)\n",data);
      DestroyVidStream(vid_stream,xinfo);
      return NULL;
    }
  } else {
#ifdef UTIL2
    vid_stream->curBits = *vid_stream->buffer << vid_stream->bit_offset;
#else
    vid_stream->curBits = *vid_stream->buffer;
#endif
  }

  /* Get next 32 bits (size of start codes). */

  show_bits32(data);

  /*
   * Process according to start code (or parse macroblock if not a start code
   * at all).
   */

  switch (data) {

  case SEQ_END_CODE:
  case 0x000001b9:   /*  handle ISO_11172_END_CODE too */

    /* Display last frame. */

    if (vid_stream->future != NULL) {
      vid_stream->current = vid_stream->future;
#ifndef NOCONTROLS
      ExecuteDisplay(vid_stream, 1, xinfo);
#else
      ExecuteDisplay(vid_stream, xinfo);
#endif
    }

    /* Sequence done. Do the right thing. For right now, exit. */
    if (!quietFlag) {
      fprintf(stderr, "\nDone!\n");
    }

#ifdef ANALYSIS
    PrintAllStats(vid_stream);
#endif
    PrintTimeInfo(vid_stream);

    vid_stream->film_has_ended=TRUE;
#ifdef NOCONTROLS
    if (loopFlag) {
      clear_data_stream(vid_stream);
    } else DestroyVidStream(vid_stream, xinfo);
#endif /* !NOCONTROLS */
    goto done;
    break;

  case SEQ_START_CODE:

    /* Sequence start code. Parse sequence header. */

    if (ParseSeqHead(vid_stream,xinfo) != PARSE_OK)
      goto error;

    /*
     * Return after sequence start code so that application above can use
     * info in header.
     */
    /*if (vid_stream->seekValue > 0) {
      SeekStream(vid_stream);      
    } */


    goto done;

  case GOP_START_CODE:

    /* Group of Pictures start code. Parse gop header. */

    if (ParseGOP(vid_stream) != PARSE_OK)
      goto error;
    goto done;

  case PICTURE_START_CODE:

    /* Picture start code. Parse picture header and first slice header. */

    status = ParsePicture(vid_stream, time_stamp);

    if (status == SKIP_PICTURE) {
      next_start_code(vid_stream);
      while (!next_bits(32, PICTURE_START_CODE, vid_stream)) {
        if (next_bits(32, GOP_START_CODE, vid_stream))
          break;
        else if (next_bits(32, SEQ_END_CODE, vid_stream))
          break;
        flush_bits(24);
        next_start_code(vid_stream);
      }
      goto done;
    } else if (status != PARSE_OK)
      goto error;


    if (ParseSlice(vid_stream) != PARSE_OK)
      goto error;
    break;

  case SEQUENCE_ERROR_CODE:
    flush_bits32;
    next_start_code(vid_stream);
    goto done;
    
  default:

    /* Check for slice start code. */

    if ((data >= SLICE_MIN_START_CODE) && (data <= SLICE_MAX_START_CODE)) {

      /* Slice start code. Parse slice header. */

      if (ParseSlice(vid_stream) != PARSE_OK)
        goto error;
    }
    break;
  }

  /* Parse next MB_QUANTUM macroblocks. */

  for (i = 0; i < MB_QUANTUM; i++) {

    /* Check to see if actually a startcode and not a macroblock. */

    if (!next_bits(23, 0x00000000, vid_stream)) {

      /* Not start code. Parse Macroblock. */

      if (ParseMacroBlock(vid_stream) != PARSE_OK)
        goto error;

#ifdef ANALYSIS
      if (showmb_flag) {
        DoDitherImage(vid_stream);
#ifndef NOCONTROLS
        ExecuteDisplay(vid_stream, 1, xinfo);
#else
        ExecuteDisplay(vid_stream, xinfo);
#endif /* !NOCONTROLS */
      }
#endif /* ANALYSIS */

    } else {

      /* Not macroblock, actually start code. Get start code. */

      next_start_code(vid_stream);
      show_bits32(data);

      /*
       * If start code is outside range of slice start codes, frame is
       * complete, display frame.
       */

      if (((data < SLICE_MIN_START_CODE) || (data > SLICE_MAX_START_CODE)) &&
	  (data != SEQUENCE_ERROR_CODE)) {

#ifdef ANALYSIS
	EndTime();
	stat_a[0].totsize += bitCount - pictureSizeCount;
	if (showEachFlag) {
	  PrintOneStat();
	};

	CollectStats();
#endif

	DoPictureDisplay(vid_stream, xinfo);
      }
      goto done;
    }
  }

  /* Check if we just finished a picture on the MB_QUANTUMth macroblock */
  if (next_bits(23, 0x00000000, vid_stream)) {
    next_start_code(vid_stream);
    show_bits32(data);
    if ((data < SLICE_MIN_START_CODE) || (data > SLICE_MAX_START_CODE)) {

#ifdef ANALYSIS
	EndTime();
	stat_a[0].totsize += bitCount - pictureSizeCount;
	if (showEachFlag) {
	  PrintOneStat();
	};

	CollectStats();
#endif

	DoPictureDisplay(vid_stream, xinfo);
    }
  }

  /* Return pointer to video stream structure. */

  goto done;

error:
  next_start_code(vid_stream);
  goto done;

done:

  return vid_stream;

}




/*
 *--------------------------------------------------------------
 *
 * ParseSeqHead --
 *
 *      Assumes bit stream is at the begining of the sequence
 *      header start code. Parses off the sequence header.
 *
 * Results:
 *      Fills the vid_stream structure with values derived and
 *      decoded from the sequence header. Allocates the pict image
 *      structures based on the dimensions of the image space
 *      found in the sequence header.
 *
 * Side effects:
 *      Bit stream irreversibly parsed off.
 *
 *--------------------------------------------------------------
 */
int
imageMpegDecoder::ParseSeqHead(
  VidStream *vid_stream,
  XInfo *xinfo)
{
  unsigned int data;
  int i, ditherType=vid_stream->ditherType;

  /* Flush off sequence start code. */

  flush_bits32;

  /* Get horizontal size of image space. */

  get_bits12(data);
  vid_stream->h_size = data;

  /* Get vertical size of image space. */

  get_bits12(data);
  vid_stream->v_size = data;

  /* Calculate macroblock width and height of image space. */

  vid_stream->mb_width = (vid_stream->h_size + 15) / 16;
  vid_stream->mb_height = (vid_stream->v_size + 15) / 16;

#ifndef DISABLE_DITHER
  /* If dither type is MBORDERED allocate ditherFlags. */
  if (ditherType == MBORDERED_DITHER) {
    vid_stream->ditherFlags =
	 (char *) malloc(vid_stream->mb_width*vid_stream->mb_height);
  }
#endif

  /*
   * Initialize ring buffer of pict images now that dimensions of image space
   * are known.
   */

#ifdef SH_MEM
  if (xinfo->display != NULL) {
    InstallXErrorHandler(xinfo->display);
  }
#endif

  if (vid_stream->ring[0] == NULL) {
    for (i = 0; i < RING_BUF_SIZE; i++) {
      vid_stream->ring[i] = NewPictImage(vid_stream,xinfo);
    }
  }
#ifdef SH_MEM
  if (xinfo->display != NULL) {
    DeInstallXErrorHandler(xinfo->display);
  }
#endif

  /* Parse of aspect ratio code. */

  get_bits4(data);
  vid_stream->aspect_ratio = (unsigned char) data;

  /* Parse off picture rate code. */

  get_bits4(data);
  vid_stream->picture_rate = (unsigned char) data;

  /* Parse off bit rate. */

  get_bits18(data);
  vid_stream->bit_rate = data;

  /* Flush marker bit. */

  flush_bits(1);

  /* Parse off vbv buffer size. */

  get_bits10(data);
  vid_stream->vbv_buffer_size = data;

#ifdef not_def
  /* Lets not bother with this.  Only increases memory image */
  if (data*1024>vid_stream->max_buf_length) {
    unsigned int *newbuf;
    int sz=1024*data+1;
    /* If they actually want a bigger buffer than we default to,
       let them have it! (if we can) */
    newbuf = (unsigned int *) realloc(vid_stream->buf_start, (unsigned int) 4*sz);
    if (newbuf!=(unsigned int *)NULL) {
      vid_stream->max_buf_length=sz;
      vid_stream->buffer=
	  (vid_stream->buffer-vid_stream->buf_start)+newbuf;
      vid_stream->buf_start=newbuf;
    }}
#endif

  /* Parse off contrained parameter flag. */

  get_bits1(data);
  if (data) {
    vid_stream->const_param_flag = TRUE;
  } else
    vid_stream->const_param_flag = FALSE;

  /*
   * If intra_quant_matrix_flag set, parse off intra quant matrix values.
   */

  get_bits1(data);
  if (data) {
    for (i = 0; i < 64; i++) {
      get_bits8(data);

      vid_stream->intra_quant_matrix[zigzag[i][1]][zigzag[i][0]] =
	(unsigned char) data;
    }
  }
  /*
   * If non intra quant matrix flag set, parse off non intra quant matrix
   * values.
   */

  get_bits1(data);
  if (data) {
    for (i = 0; i < 64; i++) {
      get_bits8(data);

      vid_stream->non_intra_quant_matrix[zigzag[i][1]][zigzag[i][0]] =
	(unsigned char) data;
    }
  }
  /* Go to next start code. */

  next_start_code(vid_stream);

  /*
   * If next start code is extension start code, parse off extension data.
   */

  if (next_bits(32, EXT_START_CODE, vid_stream)) {
    flush_bits32;
    if (vid_stream->ext_data != NULL) {
      free(vid_stream->ext_data);
      vid_stream->ext_data = NULL;
    }
    vid_stream->ext_data = get_ext_data(vid_stream);
  }
  /* If next start code is user start code, parse off user data. */

  if (next_bits(32, USER_START_CODE, vid_stream)) {
    flush_bits32;
    if (vid_stream->user_data != NULL) {
      free(vid_stream->user_data);
      vid_stream->user_data = NULL;
    }
    vid_stream->user_data = get_ext_data(vid_stream);
  }
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParseGOP --
 *
 *      Parses of group of pictures header from bit stream
 *      associated with vid_stream.
 *
 * Results:
 *      Values in gop header placed into video stream structure.
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

int
imageMpegDecoder::ParseGOP( VidStream *vid_stream)
{
  unsigned int data;

  /* Flush group of pictures start code. */

  flush_bits32;

  /* Parse off drop frame flag. */

  get_bits1(data);
  if (data) {
    vid_stream->group.drop_flag = TRUE;
  } else
    vid_stream->group.drop_flag = FALSE;

  /* Parse off hour component of time code. */

  get_bits5(data);
  vid_stream->group.tc_hours = data;

  /* Parse off minute component of time code. */

  get_bits6(data);
  vid_stream->group.tc_minutes = data;

  /* Flush marker bit. */

  flush_bits(1);

  /* Parse off second component of time code. */

  get_bits6(data);
  vid_stream->group.tc_seconds = data;

  /* Parse off picture count component of time code. */

  get_bits6(data);
  vid_stream->group.tc_pictures = data;

  /* Parse off closed gop and broken link flags. */

  get_bits2(data);
  if (data > 1) {
    vid_stream->group.closed_gop = TRUE;
    if (data > 2) {
      vid_stream->group.broken_link = TRUE;
    } else
      vid_stream->group.broken_link = FALSE;
  } else {
    vid_stream->group.closed_gop = FALSE;
    if (data) {
      vid_stream->group.broken_link = TRUE;
    } else
      vid_stream->group.broken_link = FALSE;
  }

  /* Goto next start code. */

  next_start_code(vid_stream);

  /* If next start code is extension data, parse off extension data. */

  if (next_bits(32, EXT_START_CODE, vid_stream)) {
    flush_bits32;
    if (vid_stream->group.ext_data != NULL) {
      free(vid_stream->group.ext_data);
      vid_stream->group.ext_data = NULL;
    }
    vid_stream->group.ext_data = get_ext_data(vid_stream);
  }
  /* If next start code is user data, parse off user data. */

  if (next_bits(32, USER_START_CODE,vid_stream)) {
    flush_bits32;
    if (vid_stream->group.user_data != NULL) {
      free(vid_stream->group.user_data);
      vid_stream->group.user_data = NULL;
    }
    vid_stream->group.user_data = get_ext_data(vid_stream);
  }
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParsePicture --
 *
 *      Parses picture header. Marks picture to be presented
 *      at particular time given a time stamp.
 *
 * Results:
 *      Values from picture header put into video stream structure.
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

int
imageMpegDecoder::ParsePicture( VidStream *vid_stream, TimeStamp time_stamp)
{
  unsigned int data;
  int i;

  /* Flush header start code. */
  flush_bits32;

  /* Parse off temporal reference. */
  get_bits10(data);
  vid_stream->picture.temp_ref = data;

  /* Parse of picture type. */
  get_bits3(data);
  vid_stream->picture.code_type = data;

  if ((vid_stream->picture.code_type == B_TYPE) &&
       ( (vid_stream->future == NULL) ||
       ((vid_stream->past == NULL) && !(vid_stream->group.closed_gop))))
    /* According to 2-D.5.1 (p D-18) this is ok, if the refereneces are OK */
    return SKIP_PICTURE;

  if ((vid_stream->picture.code_type == P_TYPE) &&
      ((vid_stream->future == NULL)))
    return SKIP_PICTURE;

#ifdef ANALYSIS
  StartTime();
  stat_a[0].frametype = vid_stream->picture.code_type;
  stat_a[0].number = 1;
  stat_a[0].totsize = 45;
  pictureSizeCount = bitCount;
#endif

  /* Parse off vbv buffer delay value. */
  get_bits16(data);
  vid_stream->picture.vbv_delay = data;

  /* If P or B type frame... */

  if ((vid_stream->picture.code_type == P_TYPE) || 
      (vid_stream->picture.code_type == B_TYPE)) {

    /* Parse off forward vector full pixel flag. */
    get_bits1(data);
    if (data) {
      vid_stream->picture.full_pel_forw_vector = TRUE;
    } else {
      vid_stream->picture.full_pel_forw_vector = FALSE;
    }

    /* Parse of forw_r_code. */
    get_bits3(data);

    /* Decode forw_r_code into forw_r_size and forw_f. */

    vid_stream->picture.forw_r_size = data - 1;
    vid_stream->picture.forw_f = (1 << vid_stream->picture.forw_r_size);
  }
  /* If B type frame... */

  if (vid_stream->picture.code_type == B_TYPE) {

    /* Parse off back vector full pixel flag. */
    get_bits1(data);
    if (data)
      vid_stream->picture.full_pel_back_vector = TRUE;
    else
      vid_stream->picture.full_pel_back_vector = FALSE;

    /* Parse off back_r_code. */
    get_bits3(data);

    /* Decode back_r_code into back_r_size and back_f. */

    vid_stream->picture.back_r_size = data - 1;
    vid_stream->picture.back_f = (1 << vid_stream->picture.back_r_size);
  }
  /* Get extra bit picture info. */

  if (vid_stream->picture.extra_info != NULL) {
    free(vid_stream->picture.extra_info);
    vid_stream->picture.extra_info = NULL;
  }
  vid_stream->picture.extra_info = get_extra_bit_info(vid_stream);

  /* Goto next start code. */
  next_start_code(vid_stream);

  /* If start code is extension start code, parse off extension data. */

  if (next_bits(32, EXT_START_CODE, vid_stream)) {
    flush_bits32;

    if (vid_stream->picture.ext_data != NULL) {
      free(vid_stream->picture.ext_data);
      vid_stream->picture.ext_data = NULL;
    }
    vid_stream->picture.ext_data = get_ext_data(vid_stream);
  }
  /* If start code is user start code, parse off user data. */

  if (next_bits(32, USER_START_CODE, vid_stream)) {
    flush_bits32;

    if (vid_stream->picture.user_data != NULL) {
      free(vid_stream->picture.user_data);
      vid_stream->picture.user_data = NULL;
    }
    vid_stream->picture.user_data = get_ext_data(vid_stream);
  }
  /* Find a pict image structure in ring buffer not currently locked. */

  i = 0;

  while (vid_stream->ring[i]->locked != 0) {
    if (++i >= RING_BUF_SIZE) {
      perror("Fatal error. Ring buffer full.");
      exit(1);
    }
  }

  /* Set current pict image structure to the one just found in ring. */

  vid_stream->current = vid_stream->ring[i];

  /* Set time stamp. */

  vid_stream->current->show_time = time_stamp;

  /* Reset past macroblock address field. */

  vid_stream->mblock.past_mb_addr = -1;

  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParseSlice --
 *
 *      Parses off slice header.
 *
 * Results:
 *      Values found in slice header put into video stream structure.
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

int
imageMpegDecoder::ParseSlice( VidStream *vid_stream)
{
  unsigned int data;

  /* Flush slice start code. */

  flush_bits(24);

  /* Parse off slice vertical position. */

  get_bits8(data);
  vid_stream->slice.vert_pos = data;

  /* Parse off quantization scale. */

  get_bits5(data);
  vid_stream->slice.quant_scale = data;

  /* Parse off extra bit slice info. */

  if (vid_stream->slice.extra_info != NULL) {
    free(vid_stream->slice.extra_info);
    vid_stream->slice.extra_info = NULL;
  }
  vid_stream->slice.extra_info = get_extra_bit_info(vid_stream);

  /* Reset past intrablock address. */

  vid_stream->mblock.past_intra_addr = -2;

  /* Reset previous recon motion vectors. */

  vid_stream->mblock.recon_right_for_prev = 0;
  vid_stream->mblock.recon_down_for_prev = 0;
  vid_stream->mblock.recon_right_back_prev = 0;
  vid_stream->mblock.recon_down_back_prev = 0;

  /* Reset macroblock address. */

  vid_stream->mblock.mb_address = ((vid_stream->slice.vert_pos - 1) *
				   vid_stream->mb_width) - 1;

  /* Reset past dct dc y, cr, and cb values. */

  vid_stream->block.dct_dc_y_past = 1024 << 3;
  vid_stream->block.dct_dc_cr_past = 1024 << 3;
  vid_stream->block.dct_dc_cb_past = 1024 << 3;

  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParseMacroBlock --
 *
 *      Parseoff macroblock. Reconstructs DCT values. Applies
 *      inverse DCT, reconstructs motion vectors, calculates and
 *      set pixel values for macroblock in current pict image
 *      structure.
 *
 * Results:
 *      Here's where everything really happens. Welcome to the
 *      heart of darkness.
 *
 * Side effects:
 *      Bit stream irreversibly parsed off.
 *
 *--------------------------------------------------------------
 */

int
imageMpegDecoder::ParseMacroBlock( VidStream *vid_stream)
{
  int addr_incr;
  unsigned int data;
  int mask, i, recon_right_for, recon_down_for, recon_right_back,
      recon_down_back;
  int zero_block_flag;
  BOOLEAN mb_quant = 0, mb_motion_forw = 0, mb_motion_back = 0, 
      mb_pattern = 0;
  int no_dith_flag = 0;
  int ditherType = vid_stream->ditherType;

#ifdef ANALYSIS
  mbSizeCount = bitCount;
#endif

  /*
   * Parse off macroblock address increment and add to macroblock address.
   */
  do {
    unsigned int ind;				       
    show_bits11(ind);				       
    DecodeMBAddrInc(addr_incr);
    if (mb_addr_inc[ind].num_bits==0) {
      addr_incr = 1;
    }
    if (addr_incr == MB_ESCAPE) {
      vid_stream->mblock.mb_address += 33;
      addr_incr = MB_STUFFING;
    }
  } while (addr_incr == MB_STUFFING);
  vid_stream->mblock.mb_address += addr_incr;

  if (vid_stream->mblock.mb_address > (vid_stream->mb_height *
				       vid_stream->mb_width - 1))
    return SKIP_TO_START_CODE;

  /*
   * If macroblocks have been skipped, process skipped macroblocks.
   */

  if (vid_stream->mblock.mb_address - vid_stream->mblock.past_mb_addr > 1) {
    if (vid_stream->picture.code_type == P_TYPE)
      ProcessSkippedPFrameMBlocks(vid_stream);
    else if (vid_stream->picture.code_type == B_TYPE)
      ProcessSkippedBFrameMBlocks(vid_stream);
  }
  /* Set past macroblock address to current macroblock address. */
  vid_stream->mblock.past_mb_addr = vid_stream->mblock.mb_address;

  /* Based on picture type decode macroblock type. */
  switch (vid_stream->picture.code_type) {
  case I_TYPE:
    DecodeMBTypeI(mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
		  vid_stream->mblock.mb_intra);
    break;

  case P_TYPE:
    DecodeMBTypeP(mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
		  vid_stream->mblock.mb_intra);
    break;

  case B_TYPE:
    DecodeMBTypeB(mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
		  vid_stream->mblock.mb_intra);
    break;
  case D_TYPE:
    fprintf(stderr, "ERROR:  MPEG-1 Streams with D-frames are not supported\n");
    exit(1);
  }

  /* If quantization flag set, parse off new quantization scale. */

  if (mb_quant == TRUE) {
    get_bits5(data);
    vid_stream->slice.quant_scale = data;
  }
  /* If forward motion vectors exist... */
  if (mb_motion_forw == TRUE) {

    /* Parse off and decode horizontal forward motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_h_forw_code);

    /* If horiz. forward r data exists, parse off. */

    if ((vid_stream->picture.forw_f != 1) &&
	(vid_stream->mblock.motion_h_forw_code != 0)) {
      get_bitsn(vid_stream->picture.forw_r_size, data);
      vid_stream->mblock.motion_h_forw_r = data;
    }
    /* Parse off and decode vertical forward motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_v_forw_code);

    /* If vert. forw. r data exists, parse off. */

    if ((vid_stream->picture.forw_f != 1) &&
	(vid_stream->mblock.motion_v_forw_code != 0)) {
      get_bitsn(vid_stream->picture.forw_r_size, data);
      vid_stream->mblock.motion_v_forw_r = data;
    }
  }
  /* If back motion vectors exist... */
  if (mb_motion_back == TRUE) {

    /* Parse off and decode horiz. back motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_h_back_code);

    /* If horiz. back r data exists, parse off. */

    if ((vid_stream->picture.back_f != 1) &&
	(vid_stream->mblock.motion_h_back_code != 0)) {
      get_bitsn(vid_stream->picture.back_r_size, data);
      vid_stream->mblock.motion_h_back_r = data;
    }
    /* Parse off and decode vert. back motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_v_back_code);

    /* If vert. back r data exists, parse off. */

    if ((vid_stream->picture.back_f != 1) &&
	(vid_stream->mblock.motion_v_back_code != 0)) {
      get_bitsn(vid_stream->picture.back_r_size, data);
      vid_stream->mblock.motion_v_back_r = data;
    }
  }
#ifdef ANALYSIS
  if (vid_stream->mblock.mb_intra) {
    stat_a[0].i_mbnum++;
    mbCBPPtr = stat_a[0].i_mbcbp;
    mbCoeffPtr = stat_a[0].i_mbcoeff;
    mbSizePtr = &(stat_a[0].i_mbsize);
  } else if (mb_motion_back && mb_motion_forw) {
    stat_a[0].bi_mbnum++;
    mbCBPPtr = stat_a[0].bi_mbcbp;
    mbCoeffPtr = stat_a[0].bi_mbcoeff;
    mbSizePtr = &(stat_a[0].bi_mbsize);
  } else if (mb_motion_back) {
    stat_a[0].b_mbnum++;
    mbCBPPtr = stat_a[0].b_mbcbp;
    mbCoeffPtr = stat_a[0].b_mbcoeff;
    mbSizePtr = &(stat_a[0].b_mbsize);
  } else {
    stat_a[0].p_mbnum++;
    mbCBPPtr = stat_a[0].p_mbcbp;
    mbCoeffPtr = stat_a[0].p_mbcoeff;
    mbSizePtr = &(stat_a[0].p_mbsize);
  }
#endif

  /* If mblock pattern flag set, parse and decode CBP (code block pattern). */
  if (mb_pattern == TRUE) {
    DecodeCBP(vid_stream->mblock.cbp);
  }
  /* Otherwise, set CBP to zero. */
  else
    vid_stream->mblock.cbp = 0;


#ifdef ANALYSIS
  mbCBPPtr[vid_stream->mblock.cbp]++;
#endif

  /* Reconstruct motion vectors depending on picture type. */
  if (vid_stream->picture.code_type == P_TYPE) {

    /*
     * If no forw motion vectors, reset previous and current vectors to 0.
     */

    if (!mb_motion_forw) {
      recon_right_for = 0;
      recon_down_for = 0;
      vid_stream->mblock.recon_right_for_prev = 0;
      vid_stream->mblock.recon_down_for_prev = 0;
    }
    /*
     * Otherwise, compute new forw motion vectors. Reset previous vectors to
     * current vectors.
     */

    else {
      ComputeForwVector(&recon_right_for, &recon_down_for, vid_stream);
    }
  }
  if (vid_stream->picture.code_type == B_TYPE) {

    /* Reset prev. and current vectors to zero if mblock is intracoded. */

    if (vid_stream->mblock.mb_intra) {
      vid_stream->mblock.recon_right_for_prev = 0;
      vid_stream->mblock.recon_down_for_prev = 0;
      vid_stream->mblock.recon_right_back_prev = 0;
      vid_stream->mblock.recon_down_back_prev = 0;
    } else {
      
      /* If no forw vectors, current vectors equal prev. vectors. */
      
      if (!mb_motion_forw) {
	recon_right_for = vid_stream->mblock.recon_right_for_prev;
	recon_down_for = vid_stream->mblock.recon_down_for_prev;
      }
      /*
       * Otherwise compute forw. vectors. Reset prev vectors to new values.
       */
      
      else {
	ComputeForwVector(&recon_right_for, &recon_down_for, vid_stream);
      }
      
      /* If no back vectors, set back vectors to prev back vectors. */
      
      if (!mb_motion_back) {
        recon_right_back = vid_stream->mblock.recon_right_back_prev;
        recon_down_back = vid_stream->mblock.recon_down_back_prev;
      }
      /* Otherwise compute new vectors and reset prev. back vectors. */

      else {
        ComputeBackVector(&recon_right_back, &recon_down_back, vid_stream);
      }

      /*
       * Store vector existence flags in structure for possible skipped
       * macroblocks to follow.
       */

      vid_stream->mblock.bpict_past_forw = mb_motion_forw;
      vid_stream->mblock.bpict_past_back = mb_motion_back;
    }
  }

#ifndef DISABLE_DITHER
  /* For each possible block in macroblock. */
  if (ditherType == GRAY_DITHER ||
      ditherType == GRAY2_DITHER ||
      ditherType == GRAY256_DITHER ||
      ditherType == GRAY2562_DITHER ||
      ditherType == MONO_DITHER ||
      ditherType == MONO_THRESHOLD) {
    for (mask = 32, i = 0; i < 4; mask >>= 1, i++) {

      /* If block exists... */
      if ((vid_stream->mblock.mb_intra) || (vid_stream->mblock.cbp & mask)) {
        zero_block_flag = 0;
        ParseReconBlock(i, vid_stream);
      } else {
        zero_block_flag = 1;
      }

      /* If macroblock is intra coded... */
      if (vid_stream->mblock.mb_intra) {
        ReconIMBlock(vid_stream, i);
      } else if (mb_motion_forw && mb_motion_back) {
        ReconBiMBlock(vid_stream, i, recon_right_for, recon_down_for,
		    recon_right_back, recon_down_back, zero_block_flag);
      } else if (mb_motion_forw || (vid_stream->picture.code_type == P_TYPE)) {
        ReconPMBlock(vid_stream, i, recon_right_for, recon_down_for,
		    zero_block_flag);
      } else if (mb_motion_back) {
        ReconBMBlock(vid_stream, i, recon_right_back, recon_down_back,
		    zero_block_flag);
      }
    }
    /* Kill the Chrominance blocks... */
    if ((vid_stream->mblock.mb_intra) || (vid_stream->mblock.cbp & 0x2)) {
        ParseAwayBlock(4, vid_stream);
    }
    if ((vid_stream->mblock.mb_intra) || (vid_stream->mblock.cbp & 0x1)) {
        ParseAwayBlock(5, vid_stream);
    }
  } else {
    if ((ditherType == MBORDERED_DITHER) &&
	(vid_stream->mblock.cbp == 0) &&
	(vid_stream->picture.code_type == 3) &&
	(!vid_stream->mblock.mb_intra) &&
	(!(mb_motion_forw && mb_motion_back))) {
#ifdef ENABLE_MB_ORDERED_DITHER
      MBOrderedDitherDisplayCopy(vid_stream, vid_stream->mblock.mb_address, 
		     mb_motion_forw, recon_right_for, recon_down_for, 
		     mb_motion_back, recon_right_back, recon_down_back,
		     vid_stream->past->display, vid_stream->future->display);
      vid_stream->ditherFlags[vid_stream->mblock.mb_address] = 0;
#endif
      no_dith_flag = 1;
    }
    else {
#endif
      for (mask = 32, i = 0; i < 6; mask >>= 1, i++) {
	
	/* If block exists... */
	if ((vid_stream->mblock.mb_intra) || (vid_stream->mblock.cbp & mask)) {
	  zero_block_flag = 0;
	  ParseReconBlock(i, vid_stream);
	} else {
	  zero_block_flag = 1;
	}
	
	/* If macroblock is intra coded... */
	if (vid_stream->mblock.mb_intra) {
	  ReconIMBlock(vid_stream, i);
	} else if (mb_motion_forw && mb_motion_back) {
	  ReconBiMBlock(vid_stream, i, recon_right_for, recon_down_for,
			recon_right_back, recon_down_back, zero_block_flag);
	} else if (mb_motion_forw || (vid_stream->picture.code_type == P_TYPE)) {
	  ReconPMBlock(vid_stream, i, recon_right_for, recon_down_for,
		       zero_block_flag);
	} else if (mb_motion_back) {
	  ReconBMBlock(vid_stream, i, recon_right_back, recon_down_back,
		       zero_block_flag);
	}
      }
#ifndef DISABLE_DITHER
    }
  }
#endif

#ifndef DISABLE_DITHER
  if ((ditherType == MBORDERED_DITHER) && (!no_dith_flag)) {
    if ((vid_stream->picture.code_type == 2) &&
	(vid_stream->mblock.cbp == 0) &&
	(!vid_stream->mblock.mb_intra)) {
#ifdef ENABLE_MB_ORDERED_DITHER
      MBOrderedDitherDisplayCopy(vid_stream, vid_stream->mblock.mb_address,
				 1, recon_right_for, recon_down_for,
				 0, 0, 0,
				 vid_stream->future->display,
				 (unsigned char *) NULL);
      vid_stream->ditherFlags[vid_stream->mblock.mb_address] = 0;
#endif
    }
    else {
      vid_stream->ditherFlags[vid_stream->mblock.mb_address] = 1;
    }
  }
#endif

  /* If D Type picture, flush marker bit. */
  if (vid_stream->picture.code_type == 4)
    flush_bits(1);

  /* If macroblock was intracoded, set macroblock past intra address. */
  if (vid_stream->mblock.mb_intra)
    vid_stream->mblock.past_intra_addr =
      vid_stream->mblock.mb_address;

#ifdef ANALYSIS
  *mbSizePtr += bitCount - mbSizeCount;
#endif
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ReconIMBlock --
 *
 *	Reconstructs intra coded macroblock.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

#define assertCrop(x)

void
imageMpegDecoder::ReconIMBlock(
  VidStream *vid_stream,
  int bnum)
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest;

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;


  /* If block is luminance block... */

  if (bnum < 4) {

    /* Calculate row and col values for upper left pixel of block. */

    row = mb_row * 16;
    col = mb_col * 16;
    if (bnum > 1)
      row += 8;
    if (bnum % 2)
      col += 8;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    /* Establish row size. */

    row_size = vid_stream->mb_width * 16;
  }
  /* Otherwise if block is Cr block... */
  /* Cr first because of the earlier mixup */

  else if (bnum == 5) {

    /* Set dest to Cr plane of current pict image. */

    dest = vid_stream->current->Cr;

    /* Establish row size. */

    row_size = vid_stream->mb_width * 8;

    /* Calculate row,col for upper left pixel of block. */

    row = mb_row * 8;
    col = mb_col * 8;
  }
  /* Otherwise block is Cb block, and ... */

  else {

    /* Set dest to Cb plane of current pict image. */

    dest = vid_stream->current->Cb;

    /* Establish row size. */

    row_size = vid_stream->mb_width * 8;

    /* Calculate row,col for upper left pixel value of block. */

    row = mb_row * 8;
    col = mb_col * 8;
  }

  /*
   * For each pixel in block, set to cropped reconstructed value from inverse
   * dct.
   */
  {
    short *sp = &vid_stream->block.dct_recon[0][0];
    unsigned char *cm = cropTbl + MAX_NEG_CROP;
    dest += row * row_size + col;
    for (rr = 0; rr < 4; rr++, sp += 16, dest += row_size) {
      dest[0] = cm[sp[0]];
	  assertCrop(sp[0]);
      dest[1] = cm[sp[1]];
      assertCrop(sp[1]);
      dest[2] = cm[sp[2]];
      assertCrop(sp[2]);
      dest[3] = cm[sp[3]];
      assertCrop(sp[3]);
      dest[4] = cm[sp[4]];
      assertCrop(sp[4]);
      dest[5] = cm[sp[5]];
      assertCrop(sp[5]);
      dest[6] = cm[sp[6]];
      assertCrop(sp[6]);
      dest[7] = cm[sp[7]];
      assertCrop(sp[7]);

      dest += row_size;
      dest[0] = cm[sp[8]];
      assertCrop(sp[8]);
      dest[1] = cm[sp[9]];
      assertCrop(sp[9]);
      dest[2] = cm[sp[10]];
      assertCrop(sp[10]);
      dest[3] = cm[sp[11]];
      assertCrop(sp[11]);
      dest[4] = cm[sp[12]];
      assertCrop(sp[12]);
      dest[5] = cm[sp[13]];
      assertCrop(sp[13]);
      dest[6] = cm[sp[14]];
      assertCrop(sp[14]);
      dest[7] = cm[sp[15]];
      assertCrop(sp[15]);
    }
  }
}



/*
 *--------------------------------------------------------------
 *
 * ReconPMBlock --
 *
 *	Reconstructs forward predicted macroblocks.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ReconPMBlock(
  VidStream *vid_stream,
  int bnum, int recon_right_for, int recon_down_for,int  zflag)
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest, *past;
  unsigned char *rindex1, *rindex2, *rindex3, *rindex4;
  unsigned char *index;
  short int *blockvals;

#ifdef LOOSE_MPEG
  int maxx, maxy, cc;
  int illegalBlock = 0;
  int row_start, row_end, rfirst, rlast, col_start, col_end, cfirst, clast;
#endif

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;

  if (bnum < 4) {

    /* Calculate right_for, down_for motion vectors. */

    vid_stream->right_for = recon_right_for >> 1;
    vid_stream->down_for = recon_down_for >> 1;
    vid_stream->right_half_for = recon_right_for & 0x1;
    vid_stream->down_half_for = recon_down_for & 0x1;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    if (vid_stream->picture.code_type == B_TYPE) {
      if (vid_stream->past != NULL)
       past = vid_stream->past->luminance;
    } else {

      /* Set predictive frame to current future frame. */

      if (vid_stream->future != NULL)
        past = vid_stream->future->luminance;
    }

    /* Establish row size. */

    row_size = vid_stream->mb_width << 4;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 4;
    col = mb_col << 4;
    if (bnum > 1)
      row += 8;
    if (bnum % 2)
      col += 8;

#ifdef LOOSE_MPEG
    /* Check for block illegality. */

    maxx = vid_stream->mb_width*16-1;
    maxy = vid_stream->mb_height*16-1;

    if (row + vid_stream->down_for + 7 > maxy) illegalBlock |= 0x4;
    else if (row + vid_stream->down_for < 0)  illegalBlock |= 0x1;
    
    if (col + vid_stream->right_for + 7 > maxx) illegalBlock |= 0x2;
    else if (col + vid_stream->right_for < 0) illegalBlock |= 0x8;

#endif
  }
  /* Otherwise, block is NOT luminance block, ... */

  else {

    /* Construct motion vectors. */

    recon_right_for /= 2;
    recon_down_for /= 2;
    vid_stream->right_for = recon_right_for >> 1;
    vid_stream->down_for = recon_down_for >> 1;
    vid_stream->right_half_for = recon_right_for & 0x1;
    vid_stream->down_half_for = recon_down_for & 0x1;

    /* Establish row size. */

    row_size = vid_stream->mb_width << 3;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 3;
    col = mb_col << 3;

#ifdef LOOSE_MPEG
    /* Check for block illegality. */

    maxx = vid_stream->mb_width*16-1;
    maxy = vid_stream->mb_height*16-1;

    if (row + vid_stream->down_for  + 7 > maxy) illegalBlock |= 0x4;
    else if (row + vid_stream->down_for < 0) illegalBlock |= 0x1;

    if (col + vid_stream->right_for  + 7 > maxx) illegalBlock  |= 0x2;
    else if (col + vid_stream->right_for < 0) illegalBlock |= 0x8;

#endif

    /* If block is Cr block... */
    /* 5 first because order was mixed up in earlier versions */

    if (bnum == 5) {

      /* Set dest to Cr plane of current pict image. */

      dest = vid_stream->current->Cr;

      if (vid_stream->picture.code_type == B_TYPE) {

    if (vid_stream->past != NULL)
      past = vid_stream->past->Cr;
      } else {
    if (vid_stream->future != NULL)
      past = vid_stream->future->Cr;
      }
    }
    /* Otherwise, block is Cb block... */

    else {

      /* Set dest to Cb plane of current pict image. */

      dest = vid_stream->current->Cb;

      if (vid_stream->picture.code_type == B_TYPE) {
        if (vid_stream->past != NULL)
          past = vid_stream->past->Cb;
      } else {
        if (vid_stream->future != NULL)
          past = vid_stream->future->Cb;
      }
    }
  }

  /* For each pixel in block... */

#ifdef LOOSE_MPEG

  if (illegalBlock) {
    if (illegalBlock & 0x1) {
      row_start = 0;
      row_end = row+vid_stream->down_for+8;
      rfirst = rlast = 8 - row_end;
    }
    else if (illegalBlock & 0x4) {
      row_start = row + vid_stream->down_for;
      row_end = maxy+1;
      rlast = row_end - row_start - 1;
      rfirst = 0;
    }
    else {
      row_start = row+vid_stream->down_for;
      row_end = row_start+8;
      rfirst = 0;
    }

    if (illegalBlock & 0x8) {
      col_start = 0;
      col_end = col + vid_stream->right_for + 8;
      cfirst = clast = 8 - col_end;
    }
    else if (illegalBlock & 0x2) {
      col_start = col + vid_stream->right_for;
      col_end = maxx + 1;
      clast = col_end - col_start - 1;
      cfirst = 0;
    }
    else {
      col_start = col + vid_stream->right_for;
      col_end = col_start + 8;
      cfirst = 0;
    }

    for (rr = row_start; rr < row_end; rr++) {
      rindex1 = past + (rr * row_size) + col_start;
      index = dest + ((row + rfirst) * row_size) + col + cfirst;
      for (cc = col_start; cc < col_end; cc++) {
    *index++ = *rindex1++;
      }
    }

    if (illegalBlock & 0x1) {
      for (rr = rlast -1; rr >=0; rr--) {
        index = dest + ((row + rr) * row_size) + col;
        rindex1 = dest + ((row + rlast) * row_size) + col;
        for (cc = 0; cc < 8; cc ++) {
          *index++ = *rindex1++;
        }
      }
    }
    else if (illegalBlock & 0x4) {
      for (rr = rlast+1; rr < 8; rr++) {
        index = dest + ((row + rr) * row_size) + col;
        rindex1 = dest + ((row + rlast) * row_size) + col;
        for (cc = 0; cc < 8; cc ++) {
          *index++ = *rindex1++;
        }
      }
    }

    if (illegalBlock & 0x2) {
      for (cc = clast+1; cc < 8; cc++) {
        index = dest + (row * row_size) + (col + cc);
        rindex1 = dest + (row * row_size) + (col + clast);
        for (rr = 0; rr < 8; rr++) {
          *index = *rindex1;
          index += row_size;
          rindex1 += row_size;
        }
      }
    }
    else if (illegalBlock & 0x8) {
      for (cc = clast-1; cc >= 0; cc--) {
        index = dest + (row * row_size) + (col + cc);
        rindex1 = dest + (row * row_size) + (col + clast);
        for (rr = 0; rr < 8; rr++) {
          *index = *rindex1;
          index += row_size;
          rindex1 += row_size;
        }
      }
    }

    if (!zflag) {
      for (rr = 0; rr < 8; rr++) {
        index = dest + (row*row_size) + col;
        blockvals = &(vid_stream->block.dct_recon[rr][0]);
        index[0] += blockvals[0];
        index[1] += blockvals[1];
        index[2] += blockvals[2];
        index[3] += blockvals[3];
        index[4] += blockvals[4];
        index[5] += blockvals[5];
        index[6] += blockvals[6];
        index[7] += blockvals[7];
      }
    }
  }
  else {

#endif

    index = dest + (row * row_size) + col;
    rindex1 = past + (row + vid_stream->down_for) * row_size 
	      + col + vid_stream->right_for;
    
    blockvals = &(vid_stream->block.dct_recon[0][0]);
    
    /*
     * Calculate predictive pixel value based on motion vectors and copy to
     * dest plane.
     */
    
    if ((!vid_stream->down_half_for) && (!vid_stream->right_half_for)) {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      if (!zflag)
        for (rr = 0; rr < 4; rr++) {
          index[0] = cm[(int) rindex1[0] + (int) blockvals[0]];
          index[1] = cm[(int) rindex1[1] + (int) blockvals[1]];
          index[2] = cm[(int) rindex1[2] + (int) blockvals[2]];
          index[3] = cm[(int) rindex1[3] + (int) blockvals[3]];
          index[4] = cm[(int) rindex1[4] + (int) blockvals[4]];
          index[5] = cm[(int) rindex1[5] + (int) blockvals[5]];
          index[6] = cm[(int) rindex1[6] + (int) blockvals[6]];
          index[7] = cm[(int) rindex1[7] + (int) blockvals[7]];
          index += row_size;
          rindex1 += row_size;
      
          index[0] = cm[(int) rindex1[0] + (int) blockvals[8]];
          index[1] = cm[(int) rindex1[1] + (int) blockvals[9]];
          index[2] = cm[(int) rindex1[2] + (int) blockvals[10]];
          index[3] = cm[(int) rindex1[3] + (int) blockvals[11]];
          index[4] = cm[(int) rindex1[4] + (int) blockvals[12]];
          index[5] = cm[(int) rindex1[5] + (int) blockvals[13]];
          index[6] = cm[(int) rindex1[6] + (int) blockvals[14]];
          index[7] = cm[(int) rindex1[7] + (int) blockvals[15]];
          blockvals += 16;
          index += row_size;
          rindex1 += row_size;
        }
      else {
        if (vid_stream->right_for & 0x1) {
          /* No alignment, use bye copy */
          for (rr = 0; rr < 4; rr++) {
            index[0] = rindex1[0];
            index[1] = rindex1[1];
            index[2] = rindex1[2];
            index[3] = rindex1[3];
            index[4] = rindex1[4];
            index[5] = rindex1[5];
            index[6] = rindex1[6];
            index[7] = rindex1[7];
            index += row_size;
            rindex1 += row_size;
            
            index[0] = rindex1[0];
            index[1] = rindex1[1];
            index[2] = rindex1[2];
            index[3] = rindex1[3];
            index[4] = rindex1[4];
            index[5] = rindex1[5];
            index[6] = rindex1[6];
            index[7] = rindex1[7];
            index += row_size;
            rindex1 += row_size;
          }
        } else if (vid_stream->right_for & 0x2) {
          /* Half-word bit aligned, use 16 bit copy */
          short *src = (short *)rindex1;
          short *dest = (short *)index;
          row_size >>= 1;
          for (rr = 0; rr < 4; rr++) {
            dest[0] = src[0];
            dest[1] = src[1];
            dest[2] = src[2];
            dest[3] = src[3];
            dest += row_size;
            src += row_size;
            
            dest[0] = src[0];
            dest[1] = src[1];
            dest[2] = src[2];
            dest[3] = src[3];
            dest += row_size;
            src += row_size;
          }
        } else {
          /* Word aligned, use 32 bit copy */
          int *src = (int *)rindex1;
          int *dest = (int *)index;
          row_size >>= 2;
          for (rr = 0; rr < 4; rr++) {
            dest[0] = src[0];
            dest[1] = src[1];
            dest += row_size;
            src += row_size;
            
            dest[0] = src[0];
            dest[1] = src[1];
            dest += row_size;
            src += row_size;
          }
        }
      }
    } else {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      rindex2 = rindex1 + vid_stream->right_half_for 
		+ (vid_stream->down_half_for * row_size);

      /* if one of the two is zero, then quality makes no difference */
      if ((!vid_stream->right_half_for) ||
	  (!vid_stream->down_half_for) || (!qualityFlag)) {
        
        if (!zflag) {
          for (rr = 0; rr < 4; rr++) {
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + 1) >> 1) + blockvals[0]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + 1) >> 1) + blockvals[1]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + 1) >> 1) + blockvals[2]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + 1) >> 1) + blockvals[3]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + 1) >> 1) + blockvals[4]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + 1) >> 1) + blockvals[5]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + 1) >> 1) + blockvals[6]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + 1) >> 1) + blockvals[7]];
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
        
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + 1) >> 1) + blockvals[8]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + 1) >> 1) + blockvals[9]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + 1) >> 1) + blockvals[10]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + 1) >> 1) + blockvals[11]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + 1) >> 1) + blockvals[12]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + 1) >> 1) + blockvals[13]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + 1) >> 1) + blockvals[14]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + 1) >> 1) + blockvals[15]];
            blockvals += 16;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
          }
        } else { /* zflag */
          for (rr = 0; rr < 8; rr++) {
            index[0] = (int) (rindex1[0] + rindex2[0] + 1) >> 1;
            index[1] = (int) (rindex1[1] + rindex2[1] + 1) >> 1;
            index[2] = (int) (rindex1[2] + rindex2[2] + 1) >> 1;
            index[3] = (int) (rindex1[3] + rindex2[3] + 1) >> 1;
            index[4] = (int) (rindex1[4] + rindex2[4] + 1) >> 1;
            index[5] = (int) (rindex1[5] + rindex2[5] + 1) >> 1;
            index[6] = (int) (rindex1[6] + rindex2[6] + 1) >> 1;
            index[7] = (int) (rindex1[7] + rindex2[7] + 1) >> 1;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
          }
        }
      } else { /* qualityFlag on and both vectors are non-zero */
        rindex3 = rindex1 + vid_stream->right_half_for;
        rindex4 = rindex1 + (vid_stream->down_half_for * row_size);
        if (!zflag) {
          for (rr = 0; rr < 4; rr++) {
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + rindex3[0] + rindex4[0] + 2) >> 2) + blockvals[0]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + rindex3[1] + rindex4[1] + 2) >> 2) + blockvals[1]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + rindex3[2] + rindex4[2] + 2) >> 2) + blockvals[2]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + rindex3[3] + rindex4[3] + 2) >> 2) + blockvals[3]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + rindex3[4] + rindex4[4] + 2) >> 2) + blockvals[4]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + rindex3[5] + rindex4[5] + 2) >> 2) + blockvals[5]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + rindex3[6] + rindex4[6] + 2) >> 2) + blockvals[6]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + rindex3[7] + rindex4[7] + 2) >> 2) + blockvals[7]];
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
            rindex3 += row_size;
            rindex4 += row_size;
        
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + rindex3[0] + rindex4[0] + 2) >> 2) + blockvals[8]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + rindex3[1] + rindex4[1] + 2) >> 2) + blockvals[9]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + rindex3[2] + rindex4[2] + 2) >> 2) + blockvals[10]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + rindex3[3] + rindex4[3] + 2) >> 2) + blockvals[11]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + rindex3[4] + rindex4[4] + 2) >> 2) + blockvals[12]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + rindex3[5] + rindex4[5] + 2) >> 2) + blockvals[13]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + rindex3[6] + rindex4[6] + 2) >> 2) + blockvals[14]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + rindex3[7] + rindex4[7] + 2) >> 2) + blockvals[15]];
            blockvals += 16;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
            rindex3 += row_size;
            rindex4 += row_size;
          }
        } else { /* zflag */
          for (rr = 0; rr < 8; rr++) {
            index[0] = (int) (rindex1[0] + rindex2[0] + rindex3[0] + rindex4[0] + 2) >> 2;
            index[1] = (int) (rindex1[1] + rindex2[1] + rindex3[1] + rindex4[1] + 2) >> 2;
            index[2] = (int) (rindex1[2] + rindex2[2] + rindex3[2] + rindex4[2] + 2) >> 2;
            index[3] = (int) (rindex1[3] + rindex2[3] + rindex3[3] + rindex4[3] + 2) >> 2;
            index[4] = (int) (rindex1[4] + rindex2[4] + rindex3[4] + rindex4[4] + 2) >> 2;
            index[5] = (int) (rindex1[5] + rindex2[5] + rindex3[5] + rindex4[5] + 2) >> 2;
            index[6] = (int) (rindex1[6] + rindex2[6] + rindex3[6] + rindex4[6] + 2) >> 2;
            index[7] = (int) (rindex1[7] + rindex2[7] + rindex3[7] + rindex4[7] + 2) >> 2;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
            rindex3 += row_size;
            rindex4 += row_size;
          }
        }
      }

    }
#ifdef LOOSE_MPEG
  }
#endif
}


/*
 *--------------------------------------------------------------
 *
 * ReconBMBlock --
 *
 *	Reconstructs back predicted macroblocks.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ReconBMBlock(
  VidStream *vid_stream,
  int bnum, int recon_right_back,int  recon_down_back,int  zflag)
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest, *future;
  int right_back, down_back, right_half_back, down_half_back;
  unsigned char *rindex1, *rindex2, *rindex3, *rindex4;
  unsigned char *index;
  short int *blockvals;

#ifdef LOOSE_MPEG
  int illegalBlock = 0;
  int maxx, maxy, cc;
  int row_start, row_end, rlast, rfirst, col_start, col_end, clast, cfirst;
#endif

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;

  /* If block is luminance block... */

  if (bnum < 4) {

    /* Calculate right_back, down_back motion vectors. */

    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    /*
     * If future frame exists, set future to luminance plane of future frame.
     */

    if (vid_stream->future != NULL)
      future = vid_stream->future->luminance;

    /* Establish row size. */

    row_size = vid_stream->mb_width << 4;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 4;
    col = mb_col << 4;
    if (bnum > 1)
      row += 8;
    if (bnum % 2)
      col += 8;

#ifdef LOOSE_MPEG

    /* Check for block illegality. */

    maxx = vid_stream->mb_width*16-1;
    maxy = vid_stream->mb_height*16-1;

    if (row + down_back + 7 > maxy) illegalBlock |= 0x4;
    else if (row + down_back < 0)  illegalBlock |= 0x1;
    
    if (col + right_back + 7 > maxx) illegalBlock |= 0x2;
    else if (col + right_back < 0) illegalBlock |= 0x8;

#endif

  }
  /* Otherwise, block is NOT luminance block, ... */

  else {

    /* Construct motion vectors. */

    recon_right_back /= 2;
    recon_down_back /= 2;
    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Establish row size. */

    row_size = vid_stream->mb_width << 3;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 3;
    col = mb_col << 3;

#ifdef LOOSE_MPEG

    /* Check for block illegality. */

    maxx = vid_stream->mb_width*8-1;
    maxy = vid_stream->mb_height*8-1;

    if (row + down_back + 7 > maxy) illegalBlock |= 0x4;
    else if (row + down_back < 0) illegalBlock |= 0x1;

    if (col + right_back + 7 > maxx) illegalBlock  |= 0x2;
    else if (col + right_back < 0) illegalBlock |= 0x8;

#endif

    /* If block is Cr block... */
    /* They were switched earlier, so 5 is first - eyhung */

    if (bnum == 5) {

      /* Set dest to Cr plane of current pict image. */

      dest = vid_stream->current->Cr;

      /*
       * If future frame exists, set future to Cr plane of future image.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cr;
    }
    /* Otherwise, block is Cb block... */

    else {

      /* Set dest to Cb plane of current pict image. */

      dest = vid_stream->current->Cb;

      /*
       * If future frame exists, set future to Cb plane of future frame.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cb;
    }
  }

  /* For each pixel in block do... */

#ifdef LOOSE_MPEG

  if (illegalBlock) {
    if (illegalBlock & 0x1) {
      row_start = 0;
      row_end = row+down_back+8;
      rfirst = rlast = 8 - row_end;
    }
    else if (illegalBlock & 0x4) {
      row_start = row + down_back;
      row_end = maxy+1;
      rlast = row_end - row_start - 1;
      rfirst = 0;
    }
    else {
      row_start = row+down_back;
      row_end = row_start+8;
      rfirst = 0;
    }

    if (illegalBlock & 0x8) {
      col_start = 0;
      col_end = col + right_back + 8;
      cfirst = clast = 8 - col_end;
    }
    else if (illegalBlock & 0x2) {
      col_start = col + right_back;
      col_end = maxx + 1;
      clast = col_end - col_start - 1;
      cfirst = 0;
    }
    else {
      col_start = col + right_back;
      col_end = col_start + 8;
      cfirst = 0;
    }

    for (rr = row_start; rr < row_end; rr++) {
      rindex1 = future + (rr * row_size) + col_start;
      index = dest + ((row + rfirst) * row_size) + col + cfirst;
      for (cc = col_start; cc < col_end; cc++) {
	*index++ = *rindex1++;
      }
    }

    if (illegalBlock & 0x1) {
      for (rr = rlast -1; rr >=0; rr--) {
	index = dest + ((row + rr) * row_size) + col;
	rindex1 = dest + ((row + rlast) * row_size) + col;
	for (cc = 0; cc < 8; cc ++) {
	  *index++ = *rindex1++;
	}
      }
    }
    else if (illegalBlock & 0x4) {
      for (rr = rlast+1; rr < 8; rr++) {
	index = dest + ((row + rr) * row_size) + col;
	rindex1 = dest + ((row + rlast) * row_size) + col;
	for (cc = 0; cc < 8; cc ++) {
	  *index++ = *rindex1++;
	}
      }
    }

    if (illegalBlock & 0x2) {
      for (cc = clast+1; cc < 8; cc++) {
	index = dest + (row * row_size) + (col + cc);
	rindex1 = dest + (row * row_size) + (col + clast);
	for (rr = 0; rr < 8; rr++) {
	  *index = *rindex1;
	  index += row_size;
	  rindex1 += row_size;
	}
      }
    }
    else if (illegalBlock & 0x8) {
      for (cc = clast-1; cc >= 0; cc--) {
	index = dest + (row * row_size) + (col + cc);
	rindex1 = dest + (row * row_size) + (col + clast);
	for (rr = 0; rr < 8; rr++) {
	  *index = *rindex1;
	  index += row_size;
	  rindex1 += row_size;
	}
      }
    }

    if (!zflag) {
      for (rr = 0; rr < 8; rr++) {
	index = dest + (row*row_size) + col;
	blockvals = &(vid_stream->block.dct_recon[rr][0]);
	index[0] += blockvals[0];
	index[1] += blockvals[1];
	index[2] += blockvals[2];
	index[3] += blockvals[3];
	index[4] += blockvals[4];
	index[5] += blockvals[5];
	index[6] += blockvals[6];
	index[7] += blockvals[7];
      }
    }
  }
  else {

#endif
    
    index = dest + (row * row_size) + col;
    rindex1 = future + (row + down_back) * row_size + col + right_back;

    blockvals = &(vid_stream->block.dct_recon[0][0]);

    if ((!right_half_back) && (!down_half_back)) {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      if (!zflag)
	for (rr = 0; rr < 4; rr++) {
	  index[0] = cm[(int) rindex1[0] + (int) blockvals[0]];
	  index[1] = cm[(int) rindex1[1] + (int) blockvals[1]];
	  index[2] = cm[(int) rindex1[2] + (int) blockvals[2]];
	  index[3] = cm[(int) rindex1[3] + (int) blockvals[3]];
	  index[4] = cm[(int) rindex1[4] + (int) blockvals[4]];
	  index[5] = cm[(int) rindex1[5] + (int) blockvals[5]];
	  index[6] = cm[(int) rindex1[6] + (int) blockvals[6]];
	  index[7] = cm[(int) rindex1[7] + (int) blockvals[7]];
	  index += row_size;
	  rindex1 += row_size;
	  
	  index[0] = cm[(int) rindex1[0] + (int) blockvals[8]];
	  index[1] = cm[(int) rindex1[1] + (int) blockvals[9]];
	  index[2] = cm[(int) rindex1[2] + (int) blockvals[10]];
	  index[3] = cm[(int) rindex1[3] + (int) blockvals[11]];
	  index[4] = cm[(int) rindex1[4] + (int) blockvals[12]];
	  index[5] = cm[(int) rindex1[5] + (int) blockvals[13]];
	  index[6] = cm[(int) rindex1[6] + (int) blockvals[14]];
	  index[7] = cm[(int) rindex1[7] + (int) blockvals[15]];
	  blockvals += 16;
	  index += row_size;
	  rindex1 += row_size;
	}
      else {
	if (right_back & 0x1) {
	  /* No alignment, use bye copy */
	  for (rr = 0; rr < 4; rr++) {
	    index[0] = rindex1[0];
	    index[1] = rindex1[1];
	    index[2] = rindex1[2];
	    index[3] = rindex1[3];
	    index[4] = rindex1[4];
	    index[5] = rindex1[5];
	    index[6] = rindex1[6];
	    index[7] = rindex1[7];
	    index += row_size;
	    rindex1 += row_size;
	    
	    index[0] = rindex1[0];
	    index[1] = rindex1[1];
	    index[2] = rindex1[2];
	    index[3] = rindex1[3];
	    index[4] = rindex1[4];
	    index[5] = rindex1[5];
	    index[6] = rindex1[6];
	    index[7] = rindex1[7];
	    index += row_size;
	    rindex1 += row_size;
	  }
	} else if (right_back & 0x2) {
	  /* Half-word bit aligned, use 16 bit copy */
	  short *src = (short *)rindex1;
	  short *dest = (short *)index;
	  row_size >>= 1;
	  for (rr = 0; rr < 4; rr++) {
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest[2] = src[2];
	    dest[3] = src[3];
	    dest += row_size;
	    src += row_size;
	    
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest[2] = src[2];
	    dest[3] = src[3];
	    dest += row_size;
	    src += row_size;
	  }
	} else {
	  /* Word aligned, use 32 bit copy */
	  int *src = (int *)rindex1;
	  int *dest = (int *)index;
	  row_size >>= 2;
	  for (rr = 0; rr < 4; rr++) {
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest += row_size;
	    src += row_size;
	    
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest += row_size;
	    src += row_size;
	  }
	}
      }
    } else {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      rindex2 = rindex1 + right_half_back + (down_half_back * row_size);
      if (!qualityFlag) {
        
        if (!zflag) {
          for (rr = 0; rr < 4; rr++) {
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + 1) >> 1) + blockvals[0]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + 1) >> 1) + blockvals[1]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + 1) >> 1) + blockvals[2]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + 1) >> 1) + blockvals[3]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + 1) >> 1) + blockvals[4]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + 1) >> 1) + blockvals[5]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + 1) >> 1) + blockvals[6]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + 1) >> 1) + blockvals[7]];
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
        
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + 1) >> 1) + blockvals[8]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + 1) >> 1) + blockvals[9]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + 1) >> 1) + blockvals[10]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + 1) >> 1) + blockvals[11]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + 1) >> 1) + blockvals[12]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + 1) >> 1) + blockvals[13]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + 1) >> 1) + blockvals[14]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + 1) >> 1) + blockvals[15]];
            blockvals += 16;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
          }
        } else { /* zflag */
          for (rr = 0; rr < 8; rr++) {
            index[0] = (int) (rindex1[0] + rindex2[0] + 1) >> 1;
            index[1] = (int) (rindex1[1] + rindex2[1] + 1) >> 1;
            index[2] = (int) (rindex1[2] + rindex2[2] + 1) >> 1;
            index[3] = (int) (rindex1[3] + rindex2[3] + 1) >> 1;
            index[4] = (int) (rindex1[4] + rindex2[4] + 1) >> 1;
            index[5] = (int) (rindex1[5] + rindex2[5] + 1) >> 1;
            index[6] = (int) (rindex1[6] + rindex2[6] + 1) >> 1;
            index[7] = (int) (rindex1[7] + rindex2[7] + 1) >> 1;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
          }
        }
      } else { /* qualityFlag on */
        rindex3 = rindex1 + right_half_back;
        rindex4 = rindex1 + (down_half_back * row_size);
        if (!zflag) {
          for (rr = 0; rr < 4; rr++) {
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + rindex3[0] + rindex4[0] + 2) >> 2) + blockvals[0]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + rindex3[1] + rindex4[1] + 2) >> 2) + blockvals[1]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + rindex3[2] + rindex4[2] + 2) >> 2) + blockvals[2]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + rindex3[3] + rindex4[3] + 2) >> 2) + blockvals[3]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + rindex3[4] + rindex4[4] + 2) >> 2) + blockvals[4]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + rindex3[5] + rindex4[5] + 2) >> 2) + blockvals[5]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + rindex3[6] + rindex4[6] + 2) >> 2) + blockvals[6]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + rindex3[7] + rindex4[7] + 2) >> 2) + blockvals[7]];
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
            rindex3 += row_size;
            rindex4 += row_size;
        
            index[0] = cm[((int) (rindex1[0] + rindex2[0] + rindex3[0] + rindex4[0] + 2) >> 2) + blockvals[8]];
            index[1] = cm[((int) (rindex1[1] + rindex2[1] + rindex3[1] + rindex4[1] + 2) >> 2) + blockvals[9]];
            index[2] = cm[((int) (rindex1[2] + rindex2[2] + rindex3[2] + rindex4[2] + 2) >> 2) + blockvals[10]];
            index[3] = cm[((int) (rindex1[3] + rindex2[3] + rindex3[3] + rindex4[3] + 2) >> 2) + blockvals[11]];
            index[4] = cm[((int) (rindex1[4] + rindex2[4] + rindex3[4] + rindex4[4] + 2) >> 2) + blockvals[12]];
            index[5] = cm[((int) (rindex1[5] + rindex2[5] + rindex3[5] + rindex4[5] + 2) >> 2) + blockvals[13]];
            index[6] = cm[((int) (rindex1[6] + rindex2[6] + rindex3[6] + rindex4[6] + 2) >> 2) + blockvals[14]];
            index[7] = cm[((int) (rindex1[7] + rindex2[7] + rindex3[7] + rindex4[7] + 2) >> 2) + blockvals[15]];
            blockvals += 16;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
            rindex3 += row_size;
            rindex4 += row_size;
          }
        } else { /* zflag */
          for (rr = 0; rr < 8; rr++) {
            index[0] = (int) (rindex1[0] + rindex2[0] + rindex3[0] + rindex4[0] + 2) >> 2;
            index[1] = (int) (rindex1[1] + rindex2[1] + rindex3[1] + rindex4[1] + 2) >> 2;
            index[2] = (int) (rindex1[2] + rindex2[2] + rindex3[2] + rindex4[2] + 2) >> 2;
            index[3] = (int) (rindex1[3] + rindex2[3] + rindex3[3] + rindex4[3] + 2) >> 2;
            index[4] = (int) (rindex1[4] + rindex2[4] + rindex3[4] + rindex4[4] + 2) >> 2;
            index[5] = (int) (rindex1[5] + rindex2[5] + rindex3[5] + rindex4[5] + 2) >> 2;
            index[6] = (int) (rindex1[6] + rindex2[6] + rindex3[6] + rindex4[6] + 2) >> 2;
            index[7] = (int) (rindex1[7] + rindex2[7] + rindex3[7] + rindex4[7] + 2) >> 2;
            index += row_size;
            rindex1 += row_size;
            rindex2 += row_size;
            rindex3 += row_size;
            rindex4 += row_size;
          }
        }
      }

    }
#ifdef LOOSE_MPEG
  }
#endif
}


/*
 *--------------------------------------------------------------
 *
 * ReconBiMBlock --
 *
 *	Reconstructs bidirectionally predicted macroblocks.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ReconBiMBlock(
  VidStream *vid_stream,
  int bnum, int recon_right_for, int recon_down_for, int recon_right_back, int recon_down_back,
  int zflag)
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest, *past=NULL, *future=NULL;
  int right_for, down_for, right_half_for, down_half_for;
  int right_back, down_back, right_half_back, down_half_back;
  unsigned char *index, *rindex1, *bindex1;
  short int *blockvals;
  int forw_row_start, back_row_start, forw_col_start, back_col_start;
#ifdef LOOSE_MPEG
  int lmaxx = vid_stream->mb_width*16-1;
  int lmaxy = vid_stream->mb_height*16-1;
  int cmaxx = vid_stream->mb_width*8-1;
  int cmaxy = vid_stream->mb_height*8-1;
#endif

#ifdef LOOSE_MPEG
  int illegal_forw = 0;
  int illegal_back = 0;
#endif

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;

  /* If block is luminance block... */

  if (bnum < 4) {

    /*
     * Calculate right_for, down_for, right_half_for, down_half_for,
     * right_back, down_bakc, right_half_back, and down_half_back, motion
     * vectors.
     */

    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;

    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    /* If past frame exists, set past to luminance plane of past frame. */

    if (vid_stream->past != NULL)
      past = vid_stream->past->luminance;

    /*
     * If future frame exists, set future to luminance plane of future frame.
     */

    if (vid_stream->future != NULL)
      future = vid_stream->future->luminance;

    /* Establish row size. */

    row_size = (vid_stream->mb_width << 4);

    /* Calculate row,col of upper left pixel in block. */

    row = (mb_row << 4);
    col = (mb_col << 4);
    if (bnum > 1)
      row += 8;
    if (bnum & 0x01)
      col += 8;

    forw_col_start = col + right_for;
    forw_row_start = row + down_for;

    back_col_start = col + right_back;
    back_row_start = row + down_back;

#ifdef LOOSE_MPEG

    /* Check for illegal pred. blocks. */


    if (forw_col_start+7 > lmaxx) illegal_forw = 1;
    else if (forw_col_start < 0) illegal_forw = 1;

    if (forw_row_start+7 > lmaxy) illegal_forw = 1;
    else if (forw_row_start < 0) illegal_forw = 1;

    if (back_col_start+7 > lmaxx) illegal_back = 1;
    else if (back_col_start < 0) illegal_back = 1;

    if (back_row_start+7 > lmaxy) illegal_back = 1;
    else if (back_row_start < 0) illegal_back = 1;

#endif

  }
  /* Otherwise, block is NOT luminance block, ... */

  else {

    /* Construct motion vectors. */

    recon_right_for /= 2;
    recon_down_for /= 2;
    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;

    recon_right_back /= 2;
    recon_down_back /= 2;
    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Establish row size. */

    row_size = (vid_stream->mb_width << 3);

    /* Calculate row,col of upper left pixel in block. */

    row = (mb_row << 3);
    col = (mb_col << 3);

    forw_col_start = col + right_for;
    forw_row_start = row + down_for;

    back_col_start = col + right_back;
    back_row_start = row + down_back;

#ifdef LOOSE_MPEG

    /* Check for illegal pred. blocks. */

    if (forw_col_start+7 > cmaxx) illegal_forw = 1;
    else if (forw_col_start < 0) illegal_forw = 1;

    if (forw_row_start+7 > cmaxy) illegal_forw = 1;
    else if (forw_row_start < 0) illegal_forw = 1;

    if (back_col_start+7 > cmaxx) illegal_back = 1;
    else if (back_col_start < 0) illegal_back = 1;
    
    if (back_row_start+7 > cmaxy) illegal_back = 1;
    else if (back_row_start < 0) illegal_back = 1;

#endif
    
    /* If block is Cr block... */
	/* Switched earlier, so we test Cr first - eyhung */

    if (bnum == 5) {

      /* Set dest to Cr plane of current pict image. */

      dest = vid_stream->current->Cr;

      /* If past frame exists, set past to Cr plane of past image. */

      if (vid_stream->past != NULL)
	past = vid_stream->past->Cr;

      /*
       * If future frame exists, set future to Cr plane of future image.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cr;
    }
    /* Otherwise, block is Cb block... */

    else {

      /* Set dest to Cb plane of current pict image. */

      dest = vid_stream->current->Cb;

      /* If past frame exists, set past to Cb plane of past frame. */

      if (vid_stream->past != NULL)
	past = vid_stream->past->Cb;

      /*
       * If future frame exists, set future to Cb plane of future frame.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cb;
    }
  }

  /* For each pixel in block... */

  index = dest + (row * row_size) + col;

#ifdef LOOSE_MPEG
  if (illegal_forw) 
    rindex1 = future + back_row_start * row_size + back_col_start;
  else 
#endif
    rindex1 = past + forw_row_start  * row_size + forw_col_start;

#ifdef LOOSE_MPEG
  if (illegal_back) 
    bindex1 = past + forw_row_start * row_size + forw_col_start;
  else 
#endif
    bindex1 = future + back_row_start * row_size + back_col_start;

  blockvals = (short int *) &(vid_stream->block.dct_recon[0][0]);

  {
  unsigned char *cm = cropTbl + MAX_NEG_CROP;
  if (!zflag)
    for (rr = 0; rr < 4; rr++) {
      index[0] = cm[((int) (rindex1[0] + bindex1[0]) >> 1) + blockvals[0]];
      index[1] = cm[((int) (rindex1[1] + bindex1[1]) >> 1) + blockvals[1]];
      index[2] = cm[((int) (rindex1[2] + bindex1[2]) >> 1) + blockvals[2]];
      index[3] = cm[((int) (rindex1[3] + bindex1[3]) >> 1) + blockvals[3]];
      index[4] = cm[((int) (rindex1[4] + bindex1[4]) >> 1) + blockvals[4]];
      index[5] = cm[((int) (rindex1[5] + bindex1[5]) >> 1) + blockvals[5]];
      index[6] = cm[((int) (rindex1[6] + bindex1[6]) >> 1) + blockvals[6]];
      index[7] = cm[((int) (rindex1[7] + bindex1[7]) >> 1) + blockvals[7]];
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;

      index[0] = cm[((int) (rindex1[0] + bindex1[0]) >> 1) + blockvals[8]];
      index[1] = cm[((int) (rindex1[1] + bindex1[1]) >> 1) + blockvals[9]];
      index[2] = cm[((int) (rindex1[2] + bindex1[2]) >> 1) + blockvals[10]];
      index[3] = cm[((int) (rindex1[3] + bindex1[3]) >> 1) + blockvals[11]];
      index[4] = cm[((int) (rindex1[4] + bindex1[4]) >> 1) + blockvals[12]];
      index[5] = cm[((int) (rindex1[5] + bindex1[5]) >> 1) + blockvals[13]];
      index[6] = cm[((int) (rindex1[6] + bindex1[6]) >> 1) + blockvals[14]];
      index[7] = cm[((int) (rindex1[7] + bindex1[7]) >> 1) + blockvals[15]];
      blockvals += 16;
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;
    }

  else
    for (rr = 0; rr < 4; rr++) {
      index[0] = (int) (rindex1[0] + bindex1[0]) >> 1;
      index[1] = (int) (rindex1[1] + bindex1[1]) >> 1;
      index[2] = (int) (rindex1[2] + bindex1[2]) >> 1;
      index[3] = (int) (rindex1[3] + bindex1[3]) >> 1;
      index[4] = (int) (rindex1[4] + bindex1[4]) >> 1;
      index[5] = (int) (rindex1[5] + bindex1[5]) >> 1;
      index[6] = (int) (rindex1[6] + bindex1[6]) >> 1;
      index[7] = (int) (rindex1[7] + bindex1[7]) >> 1;
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;

      index[0] = (int) (rindex1[0] + bindex1[0]) >> 1;
      index[1] = (int) (rindex1[1] + bindex1[1]) >> 1;
      index[2] = (int) (rindex1[2] + bindex1[2]) >> 1;
      index[3] = (int) (rindex1[3] + bindex1[3]) >> 1;
      index[4] = (int) (rindex1[4] + bindex1[4]) >> 1;
      index[5] = (int) (rindex1[5] + bindex1[5]) >> 1;
      index[6] = (int) (rindex1[6] + bindex1[6]) >> 1;
      index[7] = (int) (rindex1[7] + bindex1[7]) >> 1;
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;
    }
  }
}


/*
 *--------------------------------------------------------------
 *
 * ProcessSkippedPFrameMBlocks --
 *
 *	Processes skipped macroblocks in P frames.
 *
 * Results:
 *	Calculates pixel values for luminance, Cr, and Cb planes
 *      in current pict image for skipped macroblocks.
 *
 * Side effects:
 *	Pixel values in pict image changed.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ProcessSkippedPFrameMBlocks(
  VidStream *vid_stream)
{
  int row_size, half_row, mb_row, mb_col, row, col, rr;
  int addr, row_incr, half_row_incr, crow, ccol;
  int *dest, *src, *dest1, *src1;
  int ditherType=vid_stream->ditherType;

  /* Calculate row sizes for luminance and Cr/Cb macroblock areas. */

  row_size = vid_stream->mb_width << 4;
  half_row = (row_size >> 1);
  row_incr = row_size >> 2;
  half_row_incr = half_row >> 2;

  /* For each skipped macroblock, do... */

  for (addr = vid_stream->mblock.past_mb_addr + 1;
       addr < vid_stream->mblock.mb_address; addr++) {

    /* Calculate macroblock row and col. */

    mb_row = addr / vid_stream->mb_width;
    mb_col = addr % vid_stream->mb_width;

    /* Calculate upper left pixel row,col for luminance plane. */

    row = mb_row << 4;
    col = mb_col << 4;


    /* For each row in macroblock luminance plane... */

    dest = (int *)(vid_stream->current->luminance + (row * row_size) + col);
    src = (int *)(vid_stream->future->luminance + (row * row_size) + col);

    for (rr = 0; rr < 8; rr++) {

      /* Copy pixel values from last I or P picture. */

      dest[0] = src[0];
      dest[1] = src[1];
      dest[2] = src[2];
      dest[3] = src[3];
      dest += row_incr;
      src += row_incr;

      dest[0] = src[0];
      dest[1] = src[1];
      dest[2] = src[2];
      dest[3] = src[3];
      dest += row_incr;
      src += row_incr;
    }

    /*
     * Divide row,col to get upper left pixel of macroblock in Cr and Cb
     * planes.
     */

    crow = row >> 1;
    ccol = col >> 1;

    /* For each row in Cr, and Cb planes... */

    dest = (int *)(vid_stream->current->Cr + (crow * half_row) + ccol);
    src = (int *)(vid_stream->future->Cr + (crow * half_row) + ccol);
    dest1 = (int *)(vid_stream->current->Cb + (crow * half_row) + ccol);
    src1 = (int *)(vid_stream->future->Cb + (crow * half_row) + ccol);

    for (rr = 0; rr < 4; rr++) {

      /* Copy pixel values from last I or P picture. */

      dest[0] = src[0];
      dest[1] = src[1];

      dest1[0] = src1[0];
      dest1[1] = src1[1];

      dest += half_row_incr;
      src += half_row_incr;
      dest1 += half_row_incr;
      src1 += half_row_incr;

      dest[0] = src[0];
      dest[1] = src[1];

      dest1[0] = src1[0];
      dest1[1] = src1[1];

      dest += half_row_incr;
      src += half_row_incr;
      dest1 += half_row_incr;
      src1 += half_row_incr;
    }

#ifndef DISABLE_DITHER
    if (ditherType == MBORDERED_DITHER) {
#ifdef ENABLE_MB_ORDERED_DITHER
      MBOrderedDitherDisplayCopy(vid_stream, addr,
				 1, 0, 0, 0, 0, 0,
				 vid_stream->future->display,
				 (unsigned char *) NULL);
      vid_stream->ditherFlags[addr] = 0;
#endif
    }
#endif
  }

  vid_stream->mblock.recon_right_for_prev = 0;
  vid_stream->mblock.recon_down_for_prev = 0;
}





/*
 *--------------------------------------------------------------
 *
 * ProcessSkippedBFrameMBlocks --
 *
 *	Processes skipped macroblocks in B frames.
 *
 * Results:
 *	Calculates pixel values for luminance, Cr, and Cb planes
 *      in current pict image for skipped macroblocks.
 *
 * Side effects:
 *	Pixel values in pict image changed.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ProcessSkippedBFrameMBlocks(
  VidStream *vid_stream)
{
  int row_size, half_row, mb_row, mb_col, row, col, rr;
  int right_half_for = 0, down_half_for = 0;
  int c_right_half_for = 0, c_down_half_for = 0;
  int right_half_back = 0, down_half_back = 0;
  int c_right_half_back = 0, c_down_half_back = 0;
  int addr, right_for = 0, down_for = 0;
  int recon_right_for, recon_down_for;
  int recon_right_back, recon_down_back;
  int right_back = 0, down_back = 0;
  int c_right_for = 0, c_down_for = 0;
  int c_right_back = 0, c_down_back = 0;
  unsigned char forw_lum[256];
  unsigned char forw_cr[64], forw_cb[64];
  unsigned char back_lum[256], back_cr[64], back_cb[64];
  int row_incr, half_row_incr;
  int ccol, crow;
  int ditherType=vid_stream->ditherType;

  /* Calculate row sizes for luminance and Cr/Cb macroblock areas. */

  row_size = vid_stream->mb_width << 4;
  half_row = (row_size >> 1);
  row_incr = row_size >> 2;
  half_row_incr =  half_row >> 2;

  /* Establish motion vector codes based on full pixel flag. */

  if (vid_stream->picture.full_pel_forw_vector) {
    recon_right_for = vid_stream->mblock.recon_right_for_prev << 1;
    recon_down_for = vid_stream->mblock.recon_down_for_prev << 1;
  } else {
    recon_right_for = vid_stream->mblock.recon_right_for_prev;
    recon_down_for = vid_stream->mblock.recon_down_for_prev;
  }

  if (vid_stream->picture.full_pel_back_vector) {
    recon_right_back = vid_stream->mblock.recon_right_back_prev << 1;
    recon_down_back = vid_stream->mblock.recon_down_back_prev << 1;
  } else {
    recon_right_back = vid_stream->mblock.recon_right_back_prev;
    recon_down_back = vid_stream->mblock.recon_down_back_prev;
  }


  /* If only one motion vector, do display copy, else do full
     calculation. 
  */

#ifndef DISABLE_DITHER
  if (ditherType == MBORDERED_DITHER) {
    if (vid_stream->mblock.bpict_past_forw &&
	!vid_stream->mblock.bpict_past_back) {
      for (addr = vid_stream->mblock.past_mb_addr+1;
	   addr < vid_stream->mblock.mb_address; addr++) {
	
#ifdef ENABLE_MB_ORDERED_DITHER
	MBOrderedDitherDisplayCopy(vid_stream, addr,
				   1, recon_right_for, recon_down_for,
				 0, 0, 0, vid_stream->past->display,
				   vid_stream->future->display);
	vid_stream->ditherFlags[addr] = 0;
#endif
      }
      return;
    }
    if (vid_stream->mblock.bpict_past_back && 
	!vid_stream->mblock.bpict_past_forw) {
      for (addr = vid_stream->mblock.past_mb_addr+1;
	   addr < vid_stream->mblock.mb_address; addr++) {
	
#ifdef ENABLE_MB_ORDERED_DITHER
	MBOrderedDitherDisplayCopy(vid_stream, addr,
				   0, 0, 0,
				   1, recon_right_back, recon_down_back,
				   vid_stream->past->display, vid_stream->future->display);
	vid_stream->ditherFlags[addr] = 0;
#endif
      }
      return;
    }
  }
#endif

  /* Calculate motion vectors. */
  
  if (vid_stream->mblock.bpict_past_forw) {
    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;
    
    recon_right_for /= 2;
    recon_down_for /= 2;
    c_right_for = recon_right_for >> 1;
    c_down_for = recon_down_for >> 1;
    c_right_half_for = recon_right_for & 0x1;
    c_down_half_for = recon_down_for & 0x1;
    
  }
  if (vid_stream->mblock.bpict_past_back) {
    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;
    
    recon_right_back /= 2;
    recon_down_back /= 2;
    c_right_back = recon_right_back >> 1;
    c_down_back = recon_down_back >> 1;
    c_right_half_back = recon_right_back & 0x1;
    c_down_half_back = recon_down_back & 0x1;
    
  }
  /* For each skipped macroblock, do... */
  
  for (addr = vid_stream->mblock.past_mb_addr + 1;
       addr < vid_stream->mblock.mb_address; addr++) {
    
    /* Calculate macroblock row and col. */
    
    mb_row = addr / vid_stream->mb_width;
    mb_col = addr % vid_stream->mb_width;
    
    /* Calculate upper left pixel row,col for luminance plane. */
    
    row = mb_row << 4;
    col = mb_col << 4;
    crow = row / 2;
    ccol = col / 2;
    
    /* If forward predicted, calculate prediction values. */
    
    if (vid_stream->mblock.bpict_past_forw) {
      
      ReconSkippedBlock(vid_stream->past->luminance, forw_lum,
			row, col, row_size, right_for, down_for,
			right_half_for, down_half_for, 16);
      ReconSkippedBlock(vid_stream->past->Cr, forw_cr, crow,
			ccol, half_row,
			c_right_for, c_down_for, c_right_half_for, c_down_half_for, 8);
      ReconSkippedBlock(vid_stream->past->Cb, forw_cb, crow,
			ccol, half_row,
			c_right_for, c_down_for, c_right_half_for, c_down_half_for, 8);
    }
    /* If back predicted, calculate prediction values. */
    
    if (vid_stream->mblock.bpict_past_back) {
      ReconSkippedBlock(vid_stream->future->luminance, back_lum,
			row, col, row_size, right_back, down_back,
			right_half_back, down_half_back, 16);
      ReconSkippedBlock(vid_stream->future->Cr, back_cr, crow,
			ccol, half_row,
			c_right_back, c_down_back,
			c_right_half_back, c_down_half_back, 8);
      ReconSkippedBlock(vid_stream->future->Cb, back_cb, crow,
			ccol, half_row,
			c_right_back, c_down_back,
			c_right_half_back, c_down_half_back, 8);
    }
    if (vid_stream->mblock.bpict_past_forw &&
	!vid_stream->mblock.bpict_past_back) {
      
      int *dest, *dest1;
      int *src, *src1;
      dest = (int *)(vid_stream->current->luminance + (row * row_size) + col);
      src = (int *)forw_lum;
      
      for (rr = 0; rr < 16; rr++) {
	
	/* memcpy(dest, forw_lum+(rr<<4), 16);  */
	dest[0] = src[0];
	dest[1] = src[1];
	dest[2] = src[2];
	dest[3] = src[3];
	dest += row_incr;
	src += 4;
      }
      
      dest = (int *)(vid_stream->current->Cr + (crow * half_row) + ccol);
      dest1 = (int *)(vid_stream->current->Cb + (crow * half_row) + ccol);
      src = (int *)forw_cr;
      src1 = (int *)forw_cb;
      
      for (rr = 0; rr < 8; rr++) {
	/*
	 * memcpy(dest, forw_cr+(rr<<3), 8); memcpy(dest1, forw_cb+(rr<<3),
	 * 8);
	 */
	
	dest[0] = src[0];
	dest[1] = src[1];
	
	dest1[0] = src1[0];
	dest1[1] = src1[1];
	
	dest += half_row_incr;
	dest1 += half_row_incr;
	src += 2;
	src1 += 2;
      }
    } else if (vid_stream->mblock.bpict_past_back &&
	       !vid_stream->mblock.bpict_past_forw) {
      
      int *src, *src1;
      int *dest, *dest1;
      dest = (int *)(vid_stream->current->luminance + (row * row_size) + col);
      src = (int *)back_lum;
      
      for (rr = 0; rr < 16; rr++) {
	dest[0] = src[0];
	dest[1] = src[1];
	dest[2] = src[2];
	dest[3] = src[3];
	dest += row_incr;
	src += 4;
      }
      
      
      dest = (int *)(vid_stream->current->Cr + (crow * half_row) + ccol);
      dest1 = (int *)(vid_stream->current->Cb + (crow * half_row) + ccol);
      src = (int *)back_cr;
      src1 = (int *)back_cb;
      
      for (rr = 0; rr < 8; rr++) {
	/*
	 * memcpy(dest, back_cr+(rr<<3), 8); memcpy(dest1, back_cb+(rr<<3),
	 * 8);
	 */
	
	dest[0] = src[0];
	dest[1] = src[1];
	
	dest1[0] = src1[0];
	dest1[1] = src1[1];
	
	dest += half_row_incr;
	dest1 += half_row_incr;
	src += 2;
	src1 += 2;
      }
    } else {
      
      unsigned char *src1, *src2, *src1a, *src2a;
      unsigned char *dest, *dest1;
      dest = vid_stream->current->luminance + (row * row_size) + col;
      src1 = forw_lum;
      src2 = back_lum;
      
      for (rr = 0; rr < 16; rr++) {
        dest[0] = (int) (src1[0] + src2[0]) >> 1;
        dest[1] = (int) (src1[1] + src2[1]) >> 1;
        dest[2] = (int) (src1[2] + src2[2]) >> 1;
        dest[3] = (int) (src1[3] + src2[3]) >> 1;
        dest[4] = (int) (src1[4] + src2[4]) >> 1;
        dest[5] = (int) (src1[5] + src2[5]) >> 1;
        dest[6] = (int) (src1[6] + src2[6]) >> 1;
        dest[7] = (int) (src1[7] + src2[7]) >> 1;
        dest[8] = (int) (src1[8] + src2[8]) >> 1;
        dest[9] = (int) (src1[9] + src2[9]) >> 1;
        dest[10] = (int) (src1[10] + src2[10]) >> 1;
        dest[11] = (int) (src1[11] + src2[11]) >> 1;
        dest[12] = (int) (src1[12] + src2[12]) >> 1;
        dest[13] = (int) (src1[13] + src2[13]) >> 1;
        dest[14] = (int) (src1[14] + src2[14]) >> 1;
        dest[15] = (int) (src1[15] + src2[15]) >> 1;
        dest += row_size;
        src1 += 16;
        src2 += 16;
      }
      
      
      dest = vid_stream->current->Cr + (crow * half_row) + ccol;
      dest1 = vid_stream->current->Cb + (crow * half_row) + ccol;
      src1 = forw_cr;
      src2 = back_cr;
      src1a = forw_cb;
      src2a = back_cb;
      
      for (rr = 0; rr < 8; rr++) {
        dest[0] = (int) (src1[0] + src2[0]) >> 1;
        dest[1] = (int) (src1[1] + src2[1]) >> 1;
        dest[2] = (int) (src1[2] + src2[2]) >> 1;
        dest[3] = (int) (src1[3] + src2[3]) >> 1;
        dest[4] = (int) (src1[4] + src2[4]) >> 1;
        dest[5] = (int) (src1[5] + src2[5]) >> 1;
        dest[6] = (int) (src1[6] + src2[6]) >> 1;
        dest[7] = (int) (src1[7] + src2[7]) >> 1;
        dest += half_row;
        src1 += 8;
        src2 += 8;
	
        dest1[0] = (int) (src1a[0] + src2a[0]) >> 1;
        dest1[1] = (int) (src1a[1] + src2a[1]) >> 1;
        dest1[2] = (int) (src1a[2] + src2a[2]) >> 1;
        dest1[3] = (int) (src1a[3] + src2a[3]) >> 1;
        dest1[4] = (int) (src1a[4] + src2a[4]) >> 1;
        dest1[5] = (int) (src1a[5] + src2a[5]) >> 1;
        dest1[6] = (int) (src1a[6] + src2a[6]) >> 1;
        dest1[7] = (int) (src1a[7] + src2a[7]) >> 1;
        dest1 += half_row;
        src1a += 8;
        src2a += 8;
      }
    }
    
#ifndef DISABLE_DITHER
    if (ditherType == MBORDERED_DITHER) {
      vid_stream->ditherFlags[addr] = 1;
    }
#endif
  }
}





/*
 *--------------------------------------------------------------
 *
 * ReconSkippedBlock --
 *
 *	Reconstructs predictive block for skipped macroblocks
 *      in B Frames.
 *
 * Results:
 *	No return values.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::ReconSkippedBlock(
  unsigned char *source,
  unsigned char *dest,
  int row, int col, int row_size, int right, int down, int right_half, int down_half, int width)
{
  int rr;
  unsigned char *source2;

  source += ((row + down) * row_size) + col + right;

  if (width == 16) {
    if ((!right_half) && (!down_half)) {
	if (right & 0x1) {
	  /* No alignment, use bye copy */
	  for (rr = 0; rr < 16; rr++) {
	    dest[0] = source[0];
	    dest[1] = source[1];
	    dest[2] = source[2];
	    dest[3] = source[3];
	    dest[4] = source[4];
	    dest[5] = source[5];
	    dest[6] = source[6];
	    dest[7] = source[7];
	    dest[8] = source[8];
	    dest[9] = source[9];
	    dest[10] = source[10];
	    dest[11] = source[11];
	    dest[12] = source[12];
	    dest[13] = source[13];
	    dest[14] = source[14];
	    dest[15] = source[15];
	    dest += 16;
	    source += row_size;
	  }
	} else if (right & 0x2) {
	  /* Half-word bit aligned, use 16 bit copy */
	  short *src = (short *)source;
	  short *d = (short *)dest;
	  row_size >>= 1;
	  for (rr = 0; rr < 16; rr++) {
	    d[0] = src[0];
	    d[1] = src[1];
	    d[2] = src[2];
	    d[3] = src[3];
	    d[4] = src[4];
	    d[5] = src[5];
	    d[6] = src[6];
	    d[7] = src[7];
	    d += 8;
	    src += row_size;
	  }
	} else {
	  /* Word aligned, use 32 bit copy */
	  int *src = (int *)source;
	  int *d = (int *)dest;
	  row_size >>= 2;
	  for (rr = 0; rr < 16; rr++) {
	    d[0] = src[0];
	    d[1] = src[1];
	    d[2] = src[2];
	    d[3] = src[3];
	    d += 4;
	    src += row_size;
	  }
	}
    } else {
      source2 = source + right_half + (row_size * down_half);
      for (rr = 0; rr < width; rr++) {
        dest[0] = (int) (source[0] + source2[0]) >> 1;
        dest[1] = (int) (source[1] + source2[1]) >> 1;
        dest[2] = (int) (source[2] + source2[2]) >> 1;
        dest[3] = (int) (source[3] + source2[3]) >> 1;
        dest[4] = (int) (source[4] + source2[4]) >> 1;
        dest[5] = (int) (source[5] + source2[5]) >> 1;
        dest[6] = (int) (source[6] + source2[6]) >> 1;
        dest[7] = (int) (source[7] + source2[7]) >> 1;
        dest[8] = (int) (source[8] + source2[8]) >> 1;
        dest[9] = (int) (source[9] + source2[9]) >> 1;
        dest[10] = (int) (source[10] + source2[10]) >> 1;
        dest[11] = (int) (source[11] + source2[11]) >> 1;
        dest[12] = (int) (source[12] + source2[12]) >> 1;
        dest[13] = (int) (source[13] + source2[13]) >> 1;
        dest[14] = (int) (source[14] + source2[14]) >> 1;
        dest[15] = (int) (source[15] + source2[15]) >> 1;
        dest += width;
        source += row_size;
        source2 += row_size;
      }
    }
  } else {			/* (width == 8) */
    assert(width == 8);
    if ((!right_half) && (!down_half)) {
      if (right & 0x1) {
	for (rr = 0; rr < width; rr++) {
	  dest[0] = source[0];
	  dest[1] = source[1];
	  dest[2] = source[2];
	  dest[3] = source[3];
	  dest[4] = source[4];
	  dest[5] = source[5];
	  dest[6] = source[6];
	  dest[7] = source[7];
	  dest += 8;
	  source += row_size;
	}
      } else if (right & 0x02) {
	short *d = (short *)dest;
	short *src = (short *)source;
	row_size >>= 1;
	for (rr = 0; rr < width; rr++) {
	  d[0] = src[0];
	  d[1] = src[1];
	  d[2] = src[2];
	  d[3] = src[3];
	  d += 4;
	  src += row_size;
	}
      } else {
	int *d = (int *)dest;
	int *src = (int *)source;
	row_size >>= 2;
	for (rr = 0; rr < width; rr++) {
	  d[0] = src[0];
	  d[1] = src[1];
	  d += 2;
	  src += row_size;
	}
      }
    } else {
      source2 = source + right_half + (row_size * down_half);
      for (rr = 0; rr < width; rr++) {
        dest[0] = (int) (source[0] + source2[0]) >> 1;
        dest[1] = (int) (source[1] + source2[1]) >> 1;
        dest[2] = (int) (source[2] + source2[2]) >> 1;
        dest[3] = (int) (source[3] + source2[3]) >> 1;
        dest[4] = (int) (source[4] + source2[4]) >> 1;
        dest[5] = (int) (source[5] + source2[5]) >> 1;
        dest[6] = (int) (source[6] + source2[6]) >> 1;
        dest[7] = (int) (source[7] + source2[7]) >> 1;
        dest += width;
        source += row_size;
        source2 += row_size;
      }
    }
  }
}




/*
 *--------------------------------------------------------------
 *
 * DoPictureDisplay --
 *
 *	Converts image from Lum, Cr, Cb to colormap space. Puts
 *      image in lum plane. Updates past and future frame
 *      pointers. Dithers image. Sends to display mechanism.
 *
 * Results:
 *	Pict image structure locked if displaying or if frame
 *      is needed as past or future reference.
 *
 * Side effects:
 *	Lum plane pummelled.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::DoPictureDisplay(
  VidStream *vid_stream,
  XInfo *xinfo)
{



 switch(vid_stream->picture.code_type)
        {
        case    B_TYPE: {
                       	vid_stream->B_number++;
  			if (No_B_Flag) 
  				{
				int discardB=vid_stream->B_number%B_ratio;
				if (discardB==0) 
					{
					vid_stream->B_discard++;
					return;
					}
				}

			break;
                       	}
        case    P_TYPE: {
                       	vid_stream->P_number++;
  			if (No_P_Flag)
				if(vid_stream->P_number>1)
				{
  				int discardP=vid_stream->P_number%P_ratio;
				if (discardP==0) 
                			{
                			vid_stream->P_discard++;
                			return;
                			}
				}

			break;
                       	}
        case    I_TYPE: {
                       	vid_stream->I_number++;
  			if (No_I_Flag)
				{
  				int discardI=vid_stream->I_number%I_ratio;
				if (discardI==0)
                			{
                			vid_stream->I_discard++;
                			return;
                			}
				}

			break;
                       	}

        }






  /* Regulation a posteriori : si le nombre de frames par seconde  est < a B_frameRegulator, on
	degage l image */
/*
    if ( (B_frameRegulator!=0) && No_B_Flag &&
	(vid_stream->picture.code_type == B_TYPE) ) 
	   {
  	   double  spent = ReadSysClock() - vid_stream->realTimeStart;
	   double  done= (vid_stream->I_number+vid_stream->B_number+vid_stream->P_number) / spent;
	   printf("B Rate = %f\n",done);
	   if (done < ((double)(B_frameRegulator)) )
		{
		vid_stream->B_discard++;
		return;
		}
	   }



    if ( (P_frameRegulator!=0) && No_P_Flag &&
        (vid_stream->picture.code_type == P_TYPE) )
           {
           double  spent = ReadSysClock() - vid_stream->realTimeStart;
           double  done= (vid_stream->I_number+vid_stream->B_number+vid_stream->P_number) / spent;
           printf("P Rate = %f\n",done);
           if (done < ((double)(P_frameRegulator)) )
                {
                vid_stream->P_discard++;
                return;
                }
           }
*/

  /* Convert to colormap space and dither. */
  DoDitherImage(vid_stream);

  /* Update past and future references if needed. */

  if ((vid_stream->picture.code_type == I_TYPE) || (vid_stream->picture.code_type == P_TYPE)) {
    if (vid_stream->future == NULL) {
      vid_stream->future = vid_stream->current;
      vid_stream->future->locked |= FUTURE_LOCK;
    } else {
      if (vid_stream->past != NULL) {
        vid_stream->past->locked &= ~PAST_LOCK;
      }
      vid_stream->past = vid_stream->future;
      vid_stream->past->locked &= ~FUTURE_LOCK;
      vid_stream->past->locked |= PAST_LOCK;
      vid_stream->future = vid_stream->current;
      vid_stream->future->locked |= FUTURE_LOCK;
      vid_stream->current = vid_stream->past;
#ifndef NOCONTROLS
      ExecuteDisplay(vid_stream, 1, xinfo);
#else
      ExecuteDisplay(vid_stream, xinfo);
#endif /* !NOCONTROLS */
    }
  } else {
#ifndef NOCONTROLS
    ExecuteDisplay(vid_stream, 1, xinfo);
#else
    ExecuteDisplay(vid_stream, xinfo);
#endif /* !NOCONTROLS */
  }
}


