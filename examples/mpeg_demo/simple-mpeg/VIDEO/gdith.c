/* 
 * gdith.c --
 *
 *      Procedures dealing with grey-scale and mono dithering, 
 *      as well as X Windows set up procedures.
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

#include <math.h>
#include "../imageMpegLocal.H"
#include "video.h"
#include "dither.h"
#ifndef NOCONTROLS
#include "ctrlbar.h"
#endif
#include <sys/time.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 
   Changes to make the code reentrant:
      X variables now passed in xinfo: display, ximage,cmap,window, gc, etc
      De-globalized: ditherType, matched_depth, totNumFrames
      vid_stream->film_has_ended instead of FilmState

   Additional changes:
      Now can name and position each movie window individually
      DISABLE_DITHER cpp define - do not include dither code if defined
      NOFRAMECOUNT cpp define - do not count frames when running without
         controls
      Short circuit InitColorDisplay if not displaying anything
      ExistingWindow default now 0
   -lsh@cs.brown.edu (Loring Holden)
*/



/* Video rates table */
/* Cheat on Vid rates, round to 30, and use 30 if illegal value 
   Except for 9, where Xing means 15, and given their popularity, we'll
   be nice and do it */
int VidRateNum[16]={30, 24, 24, 25, 30, 30, 50, 60, 
                         60, 15, 30, 30, 30, 30, 30, 30};



/*
 *--------------------------------------------------------------
 *
 * InitColor --
 *
 *	Initialize lum, cr, and cb quantized range value arrays.
 *      Also initializes the lookup tables for the possible
 *      values of lum, cr, and cb.
 *    Color values from ITU-R BT.470-2 System B, G and SMPTE 170M
 *    see InitColorDither in 16bits.c for more
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
imageMpegDecoder::InitColor()
{
  int i, CR, CB;

  gd_L_tab    = (double *)malloc(LUM_RANGE*sizeof(double)); 
  gd_Cr_r_tab = (double *)malloc(CR_RANGE*sizeof(double));
  gd_Cr_g_tab = (double *)malloc(CR_RANGE*sizeof(double));
  gd_Cb_g_tab = (double *)malloc(CB_RANGE*sizeof(double));
  gd_Cb_b_tab = (double *)malloc(CB_RANGE*sizeof(double));

  if (gd_L_tab == NULL    || gd_Cr_r_tab == NULL ||
      gd_Cr_g_tab == NULL || gd_Cb_g_tab == NULL ||
      gd_Cb_b_tab == NULL) {
    fprintf(stderr, "Could not alloc memory in InitColor\n");
    exit(1);
  }

  for (i=0; i<LUM_RANGE; i++) {
    lum_values[i]  = ((i * 256) / (LUM_RANGE)) + (256/(LUM_RANGE*2));
    gd_L_tab[i] = lum_values[i];
    if (gammaCorrectFlag) {
      gd_L_tab[i] = GAMMA_CORRECTION(gd_L_tab[i]);
    }
  }
  
  for (i=0; i<CR_RANGE; i++) {
    register double tmp;
    if (chromaCorrectFlag) {
      tmp = ((i * 256) / (CR_RANGE)) + (256/(CR_RANGE*2));
      gd_Cr_r_tab[i] = (int) (0.419/0.299) * CHROMA_CORRECTION128D(tmp - 128.0);
      gd_Cr_g_tab[i] = (int) -(0.299/0.419) * CHROMA_CORRECTION128D(tmp - 128.0);
      cr_values[i] = CHROMA_CORRECTION256(tmp);
    } else {
      tmp = ((i * 256) / (CR_RANGE)) + (256/(CR_RANGE*2));
      gd_Cr_r_tab[i] = (int)  (0.419/0.299) * (tmp - 128.0);
      gd_Cr_g_tab[i] = (int) -(0.299/0.419) * (tmp - 128.0);
      cr_values[i] = (int) tmp;
    }
  }

  for (i=0; i<CB_RANGE; i++) {
    register double tmp;
    if (chromaCorrectFlag) {
      tmp = ((i * 256) / (CB_RANGE)) + (256/(CB_RANGE*2));
      gd_Cb_g_tab[i] = (int) -(0.114/0.331) * CHROMA_CORRECTION128D(tmp - 128.0);
      gd_Cb_b_tab[i] = (int)  (0.587/0.331) * CHROMA_CORRECTION128D(tmp - 128.0);
      cb_values[i] = CHROMA_CORRECTION256(tmp);
    } else {
      tmp = ((i * 256) / (CB_RANGE)) + (256/(CB_RANGE*2));
      gd_Cb_g_tab[i] = (int) -(0.114/0.331) * (tmp - 128.0);
      gd_Cb_b_tab[i] = (int)  (0.587/0.331) * (tmp - 128.0);
      cb_values[i] = (int) tmp;
    }
  }

}


/*
 *--------------------------------------------------------------
 *
 * ConvertColor --
 *
 *	Given a l, cr, cb tuple, converts it to r,g,b.
 *
 * Results:
 *	r,g,b values returned in pointers passed as parameters.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
void
imageMpegDecoder::ConvertColor( unsigned int l,unsigned int  cr,unsigned int  cb, unsigned char *r, unsigned char *g,unsigned char *b)
{
  double fl, fcr, fcb, fr, fg, fb;

/*
 * Old method w/o lookup table
 *
 * fl = 1.164*(((double) l)-16.0);
 * fcr =  ((double) cr) - 128.0;
 * fcb =  ((double) cb) - 128.0;
 *
 * fr = fl + (1.366 * fcr);
 * fg = fl - (0.700 * fcr) - (0.334 * fcb);
 * fb = fl + (1.732 * fcb);
 */

  fl = gd_L_tab[l];

  fr = fl + gd_Cr_r_tab[cr];
  fg = fl + gd_Cr_g_tab[cr] + gd_Cb_g_tab[cb];
  fb = fl + gd_Cb_b_tab[cb];

  if (fr < 0.0) fr = 0.0;
  else if (fr > 255.0) fr = 255.0;

  if (fg < 0.0) fg = 0.0;
  else if (fg > 255.0) fg = 255.0;

  if (fb < 0.0) fb = 0.0;
  else if (fb > 255.0) fb = 255.0;

  *r = (unsigned char) fr;
  *g = (unsigned char) fg;
  *b = (unsigned char) fb;

}

#ifdef SH_MEM

int gXErrorFlag = 0;

int HandleXError(
     Display *dpy,
     XErrorEvent *event)
{
  gXErrorFlag = 1;

  return 0;
}

int HandleXError();

void InstallXErrorHandler(
   Display *display)
{
  XSetErrorHandler(HandleXError);
  XFlush(display);
}

void DeInstallXErrorHandler(
   Display *display)
{
  XSetErrorHandler(NULL);
  XFlush(display);
}
#endif


/*
 *--------------------------------------------------------------
 *
 * ResizeDisplay --
 *
 *	Resizes display window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void imageMpegDecoder::ResizeDisplay( unsigned int w,unsigned int h, XInfo *xinfo)
{

#ifndef DISABLE_DITHER
  if (xinfo->ditherType == NO_DITHER || xinfo->ditherType == PPM_DITHER) return;
#endif

  XResizeWindow(xinfo->display, xinfo->window, w, h);
  XFlush(xinfo->display);
}


/*
 *--------------------------------------------------------------
 *
 * MakeWindow --
 *
 *	Create X Window
 *
 * Results:
 *	Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#ifdef SH_MEM
int CompletionType = -1;
#endif

int
imageMpegDecoder::MakeWindow(char* name,XInfo*  xinfo) 
{
  
  unsigned int fg, bg;
  char *hello = (xinfo->name == NULL) ? "MPEG Play" : xinfo->name;
  int screen;
/*  void CreateFullColorWindow();*/
  XVisualInfo vinfo;
  Display *display;
  
  if (xinfo == NULL) return 0;
#ifndef DISABLE_DITHER
  if ((xinfo->ditherType == NO_DITHER) ||
      (xinfo->ditherType == PPM_DITHER)) return 0;
#endif

  if (xinfo->display == NULL) {
    display = xinfo->display = XOpenDisplay(name);
    if (xinfo->display == NULL) {
      fprintf(stderr, "Cannot open display\n");
      exit(-2);
    }
  }
  display = xinfo->display;

#ifdef SH_MEM
  if(shmemFlag)
    CompletionType = XShmGetEventBase(display) + ShmCompletion;
#endif

  screen = DefaultScreen (display);
  
  /* Fill in hint structure */

  if (xinfo->hints.width == 0) {
    xinfo->hints.width = 150;
    xinfo->hints.height = 150;
  }
  xinfo->hints.x = xinfo->hints.x;
  xinfo->hints.y = xinfo->hints.y;
  xinfo->hints.flags = PPosition | PSize;
  
  /* Get some colors */
  
  bg = WhitePixel(display, screen);
  fg = BlackPixel(display, screen);

  if (xinfo->ExistingWindow) {
    /* This dowsnt work.  it used to.  why not? */
    xinfo->window = xinfo->ExistingWindow;
    if (!quietFlag) printf("Display is 0x%X, window is 0x%X\n", display, xinfo->window);
    return TRUE;
  }


  /* Make the window */
  
#ifndef DISABLE_DITHER
  if (xinfo->ditherType == FULL_COLOR_DITHER ||
      xinfo->ditherType == FULL_COLOR2_DITHER) {
#endif
    CreateFullColorWindow (xinfo);
    if (xinfo->window == 0) {
      fprintf (stderr, "-color option only valid on full color display\n");
      exit(-1);
    }
#ifndef DISABLE_DITHER
  } else {
    if (((XMatchVisualInfo (display, screen, 24, TrueColor,   &vinfo) != 0) ||
	 (XMatchVisualInfo (display, screen, 24, DirectColor, &vinfo) != 0)) &&
	 (!quietFlag)) {
      printf("\nOn 24 bit displays:  use -dither color to get full color\n\t\tordered dither is the default.\n");
    }
    if (xinfo->ditherType == MONO_DITHER ||
	xinfo->ditherType == MONO_THRESHOLD) {
      xinfo->window = XCreateSimpleWindow (display,
				    DefaultRootWindow(display),
				    xinfo->hints.x, xinfo-> hints.y,
				    xinfo->hints.width, xinfo->hints.height,
				    4, fg, bg);
      xinfo->depth = 1;
    } else {
      Visual *vis;
      XSetWindowAttributes attrib;
      unsigned long attrib_flags=0;
      
      if (!XMatchVisualInfo (display, screen, xinfo->depth = 8, PseudoColor, 
			     &vinfo)) {
	if (xinfo->ditherType != GRAY_DITHER &&
	    xinfo->ditherType != GRAY2_DITHER &&
	    xinfo->ditherType != GRAY256_DITHER &&
	    xinfo->ditherType != GRAY2562_DITHER) {
	  fprintf(stderr, "specified dither requires 8 bit display\n");
	  return 0;
	} else if (!XMatchVisualInfo(display, screen, xinfo->depth = 32,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, xinfo->depth = 24,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, xinfo->depth = 16,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, xinfo->depth = 8,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, xinfo->depth = 32,
			TrueColor, &vinfo) &&
	           !XMatchVisualInfo(display, screen, xinfo->depth = 24,
			TrueColor, &vinfo) &&
	           !XMatchVisualInfo(display, screen, xinfo->depth = 16,
			TrueColor, &vinfo)) {
	  fprintf(stderr, "- -dither gray requires at least 8 bit display\n");
	  exit(-1);
	}
      }
      
      vis = vinfo.visual;
      if (XDefaultDepthOfScreen(XDefaultScreenOfDisplay(display)) != 8) {
	attrib_flags |= CWColormap;
	attrib.colormap = XCreateColormap(display, DefaultRootWindow(display),
					  vis, AllocNone);
	xinfo->owncmFlag = TRUE; 
      }
      
      attrib.background_pixel = bg;
      attrib.border_pixel = fg;
      attrib.backing_store = NotUseful;
      attrib.save_under = False;
      attrib.background_pixel = bg;
      attrib.border_pixel = bg;
      attrib_flags |= CWBackPixel | CWBorderPixel | CWBackingStore | CWSaveUnder;
      xinfo->window = XCreateWindow (display,
			      DefaultRootWindow (display),
			      xinfo->hints.x, xinfo->hints.y,
			      xinfo->hints.width, xinfo->hints.height, 4,
			      xinfo->depth, InputOutput, vis,
			      attrib_flags, &attrib);
    }}
#endif 

  XSelectInput(display, xinfo->window, StructureNotifyMask);

  /* Tell other applications about this window */
  XSetStandardProperties (display, xinfo->window, hello,
			  hello, None, NULL, 0, &xinfo->hints);
  /* Map window. */
  
  XMapWindow(display, xinfo->window);
  /* Wait for map. */
  while (TRUE) {
    XEvent	xev;
    
    XNextEvent(display, &xev);
    if (xev.type == MapNotify && xev.xmap.event == xinfo->window) {
      break;
    }
  }
  
#ifndef NOCONTROLS
  XSelectInput(display, xinfo->window, ExposureMask | ButtonPressMask );
#else
  XSelectInput(display, xinfo->window, NoEventMask);
#endif

  return TRUE;
}
  

/*
 *--------------------------------------------------------------
 *
 * InitDisplay --
 *
 *	Initialized display, sets up colormap, etc.
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
imageMpegDecoder::InitDisplay( char *name, XInfo *xinfo)
{

  int ncolors = LUM_RANGE*CB_RANGE*CR_RANGE;
  XColor xcolor;
  int i, lum_num, cr_num, cb_num;
  unsigned char r, g, b;
  Colormap dcmap;
  Display *display;
  
#ifndef DISABLE_DITHER
  if ((xinfo->ditherType == NO_DITHER) || (xinfo->ditherType == PPM_DITHER))
     return;
#endif
  if ((noDisplayFlag) || (xinfo==NULL)) return;

  if (!MakeWindow(name, xinfo)) {
    /* Could not do that dither.  Try again if can */
#ifndef DISABLE_DITHER
    switch (xinfo->ditherType) {
    case HYBRID_DITHER:
    case HYBRID2_DITHER:
    case FS4_DITHER:
    case FS2_DITHER:
    case FS2FAST_DITHER:
    case Twox2_DITHER:
    case ORDERED_DITHER:
    case ORDERED2_DITHER:
    case MBORDERED_DITHER:
      fprintf(stderr, "trying -dither color\n");
      xinfo->ditherType = FULL_COLOR_DITHER;
      InitColorDisplay(name,xinfo);
      InitColorDither(xinfo->depth == 32);
      return;

    case GRAY_DITHER:
    case GRAY2_DITHER:
    case GRAY256_DITHER:
    case GRAY2562_DITHER:
    case FULL_COLOR_DITHER:
    case FULL_COLOR2_DITHER:
    case MONO_DITHER:
    case MONO_THRESHOLD:
    default:
      /* cant do anything */
      exit(-1);
  }
#else
    exit(-1);
#endif
  }

  if (xinfo != NULL) {
    display = xinfo->display;
    xinfo->gc = XCreateGC(display, xinfo->window, 0, 0);
    
    dcmap = xinfo->cmap = XDefaultColormap(display, DefaultScreen(display));
    
    xcolor.flags = DoRed | DoGreen | DoBlue;
    
    if (xinfo->owncmFlag) goto create_map;
  }
    retry_alloc_colors:
  for (i=0; i<ncolors; i++) {

    lum_num = (i / (CR_RANGE*CB_RANGE))%LUM_RANGE;
    cr_num = (i / CB_RANGE)%CR_RANGE;
    cb_num = i % CB_RANGE;

    ConvertColor(lum_num, cr_num, cb_num, &r, &g, &b);

    xcolor.red = r * 256;
    xcolor.green = g * 256;
    xcolor.blue = b * 256;

    if ((xinfo != NULL) && (XAllocColor(display, xinfo->cmap, &xcolor) == 0 
	&& xinfo->cmap == dcmap)) {
      int j;
      unsigned long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
        fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for (j = 0; j < i; j ++) {
        tmp_pixel = wpixel[j];
        XFreeColors(display, xinfo->cmap, &tmp_pixel, 1, 0);
      }

      create_map:
      XGetWindowAttributes(display, xinfo->window, &xwa);
      xinfo->cmap = XCreateColormap(display, xinfo->window,
				    xwa.visual, AllocNone);
      XSetWindowColormap(display, xinfo->window, xinfo->cmap);

      goto retry_alloc_colors;
    }
    pixel[i] = xcolor.pixel;
    wpixel[i] = xcolor.pixel;
  }

  xinfo->ximage = NULL;
}

#ifndef DISABLE_DITHER

/*
 *--------------------------------------------------------------
 *
 * InitGrayDisplay --
 *
 *	Initialized display for gray scale dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#ifdef ENABLE_GRAY_DITHER 

void imageMpegDecoder::InitGrayDisplay( char *name, XInfo *xinfo)
{
  int ncolors = 128;
  XColor xcolor;
  int i;
  Colormap dcmap;
  unsigned long tmp_pixels[256];
  Window window;
  Display *display;

  if (noDisplayFlag) return;
  MakeWindow(name,xinfo);
  display=xinfo->display;
  window=xinfo->window;

  xinfo->gc = XCreateGC(display, window, 0, 0);

  dcmap = xinfo->cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  if (xinfo->owncmFlag) goto create_map;
  retry_alloc_grays:
  for (i=0; i<ncolors; i++) {

    xcolor.red = xcolor.green = xcolor.blue = GAMMA_CORRECTION(i*2) * 256;

    if(XAllocColor(display, xinfo->cmap, &xcolor) == 0 
		   && xinfo->cmap == dcmap) {
      int j;
      XWindowAttributes xwa;

      if (!quietFlag) {
        fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
        unsigned long tmp_pixel;
        tmp_pixel = tmp_pixels[j*2];
        XFreeColors(display, xinfo->cmap, &tmp_pixel, 1, 0);
      }

      create_map:
      XGetWindowAttributes(display, window, &xwa);
      xinfo->cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, xinfo->cmap);

      goto retry_alloc_grays;
    }
    tmp_pixels[i*2] = pixel[i*2] = xcolor.pixel;
    tmp_pixels[(i*2)+1] = pixel[(i*2)+1] = xcolor.pixel;
    wpixel[(i*2)] = xcolor.pixel;
    wpixel[(i*2)+1] = xcolor.pixel;
    if(xinfo->depth == 8) {
      wpixel[i*2] |= wpixel[i*2] << 8;
      wpixel[i*2+1] |= wpixel[i*2+1] << 8;
    }
    if(xinfo->depth == 8 || xinfo->depth == 16) {
      wpixel[i*2] |= wpixel[i*2] << 16;
      wpixel[i*2+1] |= wpixel[i*2+1] << 16;
    }
#ifdef SIXTYFOUR_BIT
    if(xinfo->depth == 8 || xinfo->depth == 16 || xinfo->depth == 24 ||
       xinfo->depth == 32) {
      wpixel[i*2] |= wpixel[i*2] << 32;
      wpixel[i*2+1] |= wpixel[i*2+1] << 32;
    }
#endif

  }

  xinfo->ximage = NULL;
}


#endif

/*
 *--------------------------------------------------------------
 *
 * InitGray256Display --
 *
 *	Initialized display for gray scale dither with 256 levels
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#ifdef ENABLE_GRAY_DITHER 

void imageMpegDecoder::InitGray256Display( char *name, XInfo *xinfo)
{
  int ncolors = 256;
  XColor xcolor;
  int i;
  Colormap dcmap;
  int result;
  XWindowAttributes xwa;
  unsigned long tmp_pixels[256];
  Display *display;

  if (noDisplayFlag) return;
  MakeWindow(name,xinfo);
  display=xinfo->display;

  xinfo->gc = XCreateGC(display, xinfo->window, 0, 0);

  dcmap = xinfo->cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  if (xinfo->owncmFlag) {
    XGetWindowAttributes(display, xinfo->window, &xwa);
    xinfo->cmap=XCreateColormap(display, xinfo->window, xwa.visual, AllocNone);
    XSetWindowColormap(display, xinfo->window, xinfo->cmap);
  }

  retry_alloc_grays:
  for (i = 0;  i < ncolors;  i++) {
    xcolor.red = xcolor.green = xcolor.blue = GAMMA_CORRECTION(i) * 256;
    if ((result = XAllocColor(display, xinfo->cmap, &xcolor)) == 0
	 && xinfo->cmap == dcmap) {
      int j;
      unsigned long tmp_pixel;

      if (!quietFlag) {
        fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for (j = 0; j < i; j ++) {
        tmp_pixel = tmp_pixels[j];
        XFreeColors(display, xinfo->cmap, &tmp_pixel, 1, 0);
      }

      XGetWindowAttributes(display, xinfo->window, &xwa);
      xinfo->cmap = XCreateColormap(display, xinfo->window, xwa.visual,
				    AllocNone);
      XSetWindowColormap(display, xinfo->window, xinfo->cmap);

      goto retry_alloc_grays;
    }
    tmp_pixels[i] = pixel[i] = xcolor.pixel;
    wpixel[i] = xcolor.pixel;
#ifndef DISABLE_DITHER
    if(xinfo->depth == 8)
#endif
      wpixel[i] |= wpixel[i] << 8;
#ifndef DISABLE_DITHER
    if(xinfo->depth == 8 || xinfo->depth == 16) {
#endif
      wpixel[i] |= wpixel[i] << 16;
#ifndef DISABLE_DITHER
    }
#endif
#ifdef SIXTYFOUR_BIT
    if(xinfo->depth == 8 || xinfo->depth == 16 || xinfo->depth == 24 
       || xinfo->depth == 32) {
      wpixel[i] |= wpixel[i] << 32;
    }
#endif

  }

  xinfo->ximage = NULL;
}


#endif

/*
 *--------------------------------------------------------------
 *
 * InitMonoDisplay --
 *
 *	Initialized display for monochrome dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#ifdef ENABLE_MONO_DITHER 

void imageMpegDecoder::InitMonoDisplay( char *name, XInfo *xinfo)
{
  XGCValues xgcv;
  Display *display;

  if (noDisplayFlag) return;
  MakeWindow(name,xinfo);
  display=xinfo->display;

  xgcv.background = BlackPixel(display, DefaultScreen(display));
  xgcv.foreground = WhitePixel(display, DefaultScreen(display));

  xinfo->gc = XCreateGC(display, xinfo->window,
			GCForeground | GCBackground,
			&xgcv);

  xinfo->ximage = NULL;
}
#endif


#endif


/*
 *--------------------------------------------------------------
 *
 * InitColorDisplay --
 *
 *	Initialized display for full color output.
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
imageMpegDecoder::InitColorDisplay( char *name, XInfo *xinfo)
{
  XWindowAttributes winattr;

#ifndef DISABLE_DITHER
  if ((xinfo->ditherType == NO_DITHER) || (xinfo->ditherType == PPM_DITHER))
     return;
#endif
  if ((noDisplayFlag) || (xinfo==NULL)) return;

  MakeWindow(name,xinfo);

  if (xinfo->gc==0) {
    xinfo->gc = XCreateGC(xinfo->display, xinfo->window, 0, 0);
  }
  xinfo->ximage = NULL;

  XGetWindowAttributes(xinfo->display, xinfo->window, &winattr);
  /*
   * Misuse of wpixel 
   */
  wpixel[0] = winattr.visual->red_mask;
  wpixel[1] = winattr.visual->green_mask;
  wpixel[2] = winattr.visual->blue_mask;
}


/*
 *--------------------------------------------------------------
 *
 * ExecuteDisplay --
 *
 *	Actually displays display plane in previously created window.
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
#ifndef NOCONTROLS
imageMpegDecoder::ExecuteDisplay( VidStream *vid_stream, int frame_increment, XInfo *xinfo)
#else
imageMpegDecoder::ExecuteDisplay( VidStream *vid_stream,  XInfo *xinfo)
#endif
{
  char dummy;
  Visual *fc_visual;
  int depth, result;
  register int usec, sec;
#ifndef NOCONTROLS
  XEvent xev;
#endif
  Display *display;
  

  if(No_D_Flag)
        {
        int discard=(vid_stream->I_number+vid_stream->P_number+vid_stream->B_number-vid_stream->I_discard-vid_stream->P_discard-vid_stream->B_discard)%D_ratio;
        if (discard==0)
                {
                vid_stream->D_discard++;
                return;
                }
        }




  if (xinfo!=NULL) display=xinfo->display;

#ifndef NOCONTROLS
  vid_stream->totNumFrames+=frame_increment;
#else
  vid_stream->totNumFrames++;
#endif

  if (partialFlag) {
    if ((endFrame != -1) && (vid_stream->totNumFrames > endFrame)) {
#ifdef ANALYSIS
      PrintAllStats(vid_stream);
#endif
      PrintTimeInfo(vid_stream);

      vid_stream->film_has_ended=TRUE;
#ifndef NOCONTROLS
      if ((frame_increment != 0) && (ControlShow != CTRLBAR_NONE))
        UpdateFrameTotal(xinfo->display);
      return;
#else
      if (loopFlag) {
              clear_data_stream(vid_stream);
      } else DestroyVidStream(vid_stream, xinfo);
      return;
#endif
    }
    if (vid_stream->totNumFrames < startFrame) {
      return;
    }
  }


  /* Do frame rate control */
  switch (rate_deal) {
  case 0:
    break;
  default:
    gettimeofday(&tfnow, (struct timezone *)NULL);
    usec = tftarget.tv_usec - tfnow.tv_usec;
    sec  = tftarget.tv_sec - tfnow.tv_sec;
    if (usec < 0) {
      usec += 1000000;
      sec--;
    }
    
    /* If we're not behind, wait a bit */
    if ((sec >= 0)  &&  usec > 0) {
      tfnow.tv_sec = sec;
      tfnow.tv_usec = usec;
      select(0, NULL, NULL, NULL ,&tfnow); 
      gettimeofday(&tfnow, (struct timezone *)NULL);
    }
    /* Setup target for next frame */
    tftarget.tv_usec = tfnow.tv_usec + one_frame_time;
    if (tftarget.tv_usec >= 1000000) {
      tftarget.tv_usec -= 1000000;
      tftarget.tv_sec = tfnow.tv_sec + 1;
    } else tftarget.tv_sec = tfnow.tv_sec;
    break;
  case -1:
    switch (framerate) {
    case -1: /* Go with stream Value */
      rate_deal = VidRateNum[vid_stream->picture_rate];
      gettimeofday(&tftarget, (struct timezone *)NULL);
      one_frame_time = 1000000 / rate_deal;
      break;
    case 0: /* as fast as possible */
      rate_deal = 0;
      break;
    default:
      rate_deal = framerate;
      gettimeofday(&tftarget, (struct timezone *)NULL);
      one_frame_time = 1000000 / rate_deal;
      break;
    }
    break;
  }

#ifndef NOCONTROLS 
  if (frame_increment > 0) {
    TotalFrameCount++;
    if ((endFrame != -1) && (vid_stream->totNumFrames >= endFrame)) {
      vid_stream->film_has_ended=TRUE;
    }
    if (!quietFlag && ControlShow == CTRLBAR_NONE)
      fprintf(stderr, "%d\r", vid_stream->totNumFrames);
  }
#else
#ifndef NOFRAMECOUNT
  if (!quietFlag) {
    fprintf (stderr, "%d\r", vid_stream->totNumFrames);
  }
#endif
#endif

#ifndef DISABLE_DITHER
  if (vid_stream->ditherType == NO_DITHER) return;

#ifdef ENABLE_PPM
  if (vid_stream->ditherType == PPM_DITHER) {
    ExecutePPM(vid_stream);
    return;
  }
#endif
#endif
  if (xinfo==NULL) return;
  if (!noDisplayFlag) {
  if (xinfo->ximage == NULL) {

    int pixels_per_mb = 16;
#ifndef DISABLE_DITHER
    if(IS_2x2_DITHER(xinfo->ditherType)) pixels_per_mb = 32;
    
    if ((xinfo->ditherType == FULL_COLOR_DITHER) ||
	(xinfo->ditherType == FULL_COLOR2_DITHER)) {
#endif
      int w, h;
      
      w = vid_stream->mb_width  * pixels_per_mb;
      h = vid_stream->mb_height * pixels_per_mb;
      
      fc_visual = FindFullColorVisual(display, &depth);
      xinfo->ximage = XCreateImage (display,
			     fc_visual, depth, ZPixmap,
			     0, &dummy, w, h, 32, 0);
      
#ifndef DISABLE_DITHER
    } else if (xinfo->ditherType == MONO_DITHER
	    || xinfo->ditherType == MONO_THRESHOLD) {
      xinfo->ximage = XCreateImage (display,
			     None, xinfo->depth, XYBitmap, 0, &dummy,
			     vid_stream->mb_width * pixels_per_mb,
			     vid_stream->mb_height * pixels_per_mb, 8, 0);
      xinfo->ximage->byte_order = MSBFirst;
      xinfo->ximage->bitmap_bit_order = MSBFirst;
    } else {
      xinfo->ximage = XCreateImage(display,
		       None, xinfo->depth, ZPixmap, 0, &dummy,
			    vid_stream->mb_width * pixels_per_mb,
			    vid_stream->mb_height * pixels_per_mb, 8, 0);
    }
#endif
  }

/* 
 * Always work in native bit and byte order. This tells Xlib to reverse
 * bit and byte order if necessary when crossing a network. Frankly, this
 * part of XImages is somewhat underdocumented, so this may not be exactly
 * correct.
 */ 
#ifdef LITTLE_ENDIAN_ARCHITECTURE
    xinfo->ximage->byte_order = LSBFirst;
    xinfo->ximage->bitmap_bit_order = LSBFirst;
#else
    xinfo->ximage->byte_order = MSBFirst;
    xinfo->ximage->bitmap_bit_order = MSBFirst;
#endif

  
#ifdef SH_MEM
    if (shmemFlag) {
           XShmPutImage(display,
			xinfo->window,
			xinfo->gc, vid_stream->current->ximage, 
                        0, 0, 0, 0,
                        vid_stream->current->ximage->width, 
                        vid_stream->current->ximage->height, True);
           XFlush(display);
      
#ifndef NOCONTROLS
      /* Wait for it _without_ removing other events from queue */
      XIfEvent(display, &xev, IfEventType, (char *) (&CompletionType));
#else
      while(1) {
        XEvent xev;
	
        XNextEvent(display, &xev);
        if (xev.type == CompletionType) {
          break;
        }
      }
#endif
    }
    else 
#endif
      
      {
        xinfo->ximage->data = (char *) vid_stream->current->display; 
	
        result=XPutImage(display, xinfo->window, xinfo->gc, xinfo->ximage,
		  0, 0, 0, 0,
		  xinfo->ximage->width,
		  xinfo->ximage->height);
      }
  }
#ifndef NOCONTROLS
      if ((frame_increment != 0) && (ControlShow != CTRLBAR_NONE)) {
        UpdateFrameTotal(xinfo->display);
      }
#endif

}

#ifndef DISABLE_DITHER
extern char *strrchr();
#define PPM_BITS 8


/*
 *--------------------------------------------------------------
 *
 * ExecutePPM --
 *
 *	Write out a display plane as a PPM file.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */



#ifdef ENABLE_PPM_DITHER

void
imageMpegDecoder::ExecutePPM( VidStream *vid_stream)
{
  char fileName[300], cmd[400];
  FILE *file;
  int n;
  unsigned int *p;
  unsigned int r, g, b;

  if (!munged) {
    char *cp;

    cp = strrchr(vid_stream->filename, '/');
    if (cp != NULL)
      ++cp;
    else
      cp = vid_stream->filename;
    strcpy(mungedInputName, cp);
    cp = strrchr(mungedInputName, '.');
    if (cp != NULL)
	*cp = '\0';
    munged = 1;
  }

  sprintf(fileName, "%s_%05d.ppm", mungedInputName, vid_stream->totNumFrames);
  file = fopen(fileName, "w");
  if (vid_stream->ppm_width != -1 && vid_stream->ppm_height != -1) {
    if ((vid_stream->ppm_modulus != -1) &&
	((vid_stream->totNumFrames-1) % vid_stream->ppm_modulus != 0)) {
      return;
    } else {
      
      sprintf(cmd, "pnmscale -xysize %d %d > %s",
 	      vid_stream->ppm_width, vid_stream->ppm_height, fileName);
      file = popen(cmd, "w");
    }
  } else {
    file = fopen(fileName, "w");
  }
  if (file == NULL) {
    perror(fileName);
    exit(1);
  }

  fprintf(file, "P6\n");
  fprintf(file, "%d %d\n", vid_stream->h_size, vid_stream->v_size);
  fprintf(file, "255\n");

  p = (unsigned int *) vid_stream->current->display;
  n = vid_stream->h_size * vid_stream->v_size;
  while (n > 0) {
    r = *p & 0xff;
    g = (*p >> PPM_BITS) & 0xff;
    b = (*p >> (2*PPM_BITS)) & 0xff;
    putc(r, file);
    putc(g, file);
    putc(b, file);
    ++p;
    --n;
  }

  if (vid_stream->ppm_width != -1 && vid_stream->ppm_height != -1) {
    pclose(file);
  } else {
    fclose(file);
  }
}
#endif
#endif


#ifdef NO_GETTIMEOFDAY
/* raw approximation */
int imageMpegDecoder::gettimeofday (struct timeval * retval, void * unused)
{
	timeb_t tm;

	ftime (&tm);
	retval->tv_sec=  tm.time;
	retval->tv_usec= tm.millitm*1000;
	return 0;
}
#endif
