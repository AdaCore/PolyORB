/* 
 * util32.c --
 *
 *      Miscellaneous functions that deal with 32 bit color displays.
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
#include "../imageMpegLocal.H"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "video.h"

/*
   Changes to make the code reentrant:
      deglobalized: matched_depth
      use X variables in xinfo instead of globals
   Additional changes:
      #ifdef DISABLE_DITHER  - don't compile dither code
      fix parameter types for XCreateWindow call
   -lsh@cs.brown.edu (Loring Holden)
 */


/*
 *--------------------------------------------------------------
 *
 * FindFullColorVisual
 *
 *  Returns a pointer to a full color bit visual on the display
 *
 * Results:
 *      See above.
 *  
 * Side effects:
 *      Unknown.
 *
 *--------------------------------------------------------------
 */
Visual *
imageMpegDecoder::FindFullColorVisual (
     Display *dpy,
     int *depth)
{
  XVisualInfo vinfo;
  XVisualInfo *vinfo_ret;
  int numitems, maxdepth;
  
  vinfo.c_class = TrueColor;
  
  vinfo_ret = XGetVisualInfo(dpy, VisualClassMask, &vinfo, &numitems);
  
  if (numitems == 0) return NULL;

  maxdepth = 0;
  while(numitems > 0) {
    if (vinfo_ret[numitems-1].depth > maxdepth) {
      maxdepth = vinfo_ret[numitems-1 ].depth;
    }
    numitems--;
  }
  XFree((void *) vinfo_ret);

  if (maxdepth < 16) return NULL;

  if (XMatchVisualInfo(dpy, DefaultScreen(dpy), maxdepth, 
		       TrueColor, &vinfo)) {
    *depth = maxdepth;
    return vinfo.visual;
  }
  
  return NULL;
}


/*
 *--------------------------------------------------------------
 *
 * CreateFullColorWindow
 *
 *  Creates a window capable of handling 32 bit color.
 *
 * Results:
 *      See above.
 *  
 * Side effects:
 *      Unknown.
 *
 *--------------------------------------------------------------
 */
void
imageMpegDecoder::CreateFullColorWindow (XInfo* xinfo)
{
  int depth;
  Visual *visual;
  XSetWindowAttributes xswa;
  unsigned long mask;
  unsigned int cla;
  int screen;
  Display *dpy=xinfo->display;
  int x = xinfo->hints.x,
      y = xinfo->hints.y;
  unsigned int w = xinfo->hints.width,
               h = xinfo->hints.height;

  screen = XDefaultScreen(dpy);
  cla = InputOutput;	/* Could be InputOnly */
  if (xinfo->visual == NULL) {
    xinfo->visual = visual = FindFullColorVisual (dpy, &depth);
    xinfo->depth = depth;
  } else {
     visual=xinfo->visual;
     depth=xinfo->depth;
  }

  if (visual == NULL) {
    return;
  }
  mask = CWBackPixel | CWColormap | CWBorderPixel;
  if (xinfo->cmap==0) {
  xswa.colormap = XCreateColormap(dpy,
				  XRootWindow(dpy, screen),
                                  visual, AllocNone);
  } else xswa.colormap = xinfo->cmap;
  xswa.background_pixel = BlackPixel(dpy, DefaultScreen(dpy));
  xswa.border_pixel = WhitePixel(dpy, DefaultScreen(dpy));

  xinfo->window = XCreateWindow(dpy, RootWindow(dpy, screen), x, y, w, h,
    (unsigned int) 1, depth, cla, visual, mask, &xswa);
}
