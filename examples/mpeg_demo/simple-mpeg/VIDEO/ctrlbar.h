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

 contributed by (but no rights held by):

 Michael J. Donahue
 National Institute of Standards and Technology
 Gaithersburg MD USA
 donahue@ulexite.nist.gov

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


#ifndef CTRLBAR_H
#define CTRLBAR_H


#include <X11/Xlib.h>
#include <X11/Xutil.h>

/*
   Changes to make the code reentrant:
     deglobalize totNumFrames, realTimeStart, seekValue, EOF_flag
   Additional changes:
     none
   -lsh@cs.brown.edu (Loring Holden)
*/

#define DISPLAY_WINDOW_BORDER 4

/* Control bar display flag */
#define CTRLBAR_OFF  0
#define CTRLBAR_ON   1
#define CTRLBAR_NONE 2  /* No control bar at all */

/* Video play states */
#define CTRL_UNDEFINED  -1
#define CTRL_PAUSE       0
#define CTRL_PLAY        1
#define CTRL_STEP        2
#define CTRL_EOF         3
#define CTRL_REWIND      4
#define CTRL_FFWD        5   /* Fast Forward (skips frames w/o display) */
#define CTRL_EXIT        6

/* ControlMotion tracks Play/Pause status (for resumption after e.g. Rewind) */
#define CTRLMOTION_OFF 0   /* Pause */
#define CTRLMOTION_ON  1   /* Play  */

/* StopWatch constants */
#define STOPWATCH_RESET 0  /* Resets timing, returns 0.0.                   */
#define STOPWATCH_START 1  /* Starts timing, returns 0.0.                   */
#define STOPWATCH_STOP  2  /* Stops timing,  returns elapsed time in secs.  */
#define STOPWATCH_READ  3  /* Returns elapsed time in seconds.              */

#endif
