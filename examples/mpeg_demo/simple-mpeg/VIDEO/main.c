 

#include "../imageMpegLocal.H"



/*
 *--------------------------------------------------------------
 *
 * getposition --
 *
 *--------------------------------------------------------------
 */
void imageMpegDecoder::getposition( char *arg, int *xpos, int *ypos)
{
  char *pos;

  if ((pos = strtok(arg, "+-")) != NULL) {
    *xpos = atoi(pos);
    if ((pos = strtok(NULL, "+-")) != NULL) {
      *ypos = atoi(pos);
      return;
    }
  }
  if (!quietFlag) {
    fprintf(stderr, "Illegal position... Warning: argument ignored! (-position +x+y)\n");
  }
  return;
}


 

/*
 *--------------------------------------------------------------
 *
 * DoDitherImage --
 *
 *      Called when image needs to be dithered. Selects correct
 *      dither routine based on info in xinfo[0].ditherType.
 *
 * Results:
 *        None.
 *
 * Side effects:
 *        None.
 *
 *--------------------------------------------------------------
 */

void
imageMpegDecoder::DoDitherImage(VidStream * vid_stream)
{
 unsigned char *l=vid_stream->current->luminance,
               *Cr=vid_stream->current->Cr,
               *Cb=vid_stream->current->Cb,
               *disp=vid_stream->current->display;
 int h=(int) vid_stream->mb_height * 16;
 int w=(int) vid_stream->mb_width * 16;
 int ditherType=vid_stream->ditherType;
 int matched_depth=vid_stream->matched_depth;

#ifndef DISABLE_DITHER
  switch(ditherType) {

#ifdef ENABLE_HYBRID_DITHER
  case HYBRID_DITHER:
    HybridDitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif


#ifdef ENABLE_HYBRIDERR_DITHER
  case HYBRID2_DITHER:
    HybridErrorDitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif


#ifdef ENABLE_FS2FAST_DITHER
  case FS2FAST_DITHER:
    FS2FastDitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif


#ifdef ENABLE_FS2_DITHER
  case FS2_DITHER:
    FS2DitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif

#ifdef ENABLE_FS4_DITHER
  case FS4_DITHER:
    FS4DitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif


#ifdef ENABLE_2x2_DITHER
  case Twox2_DITHER:
    Twox2DitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif

  case FULL_COLOR2_DITHER:
    if (matched_depth == 32)
      Twox2Color32DitherImage(l, Cr, Cb, disp, h, w);
    else
      Twox2Color16DitherImage(l, Cr, Cb, disp, h, w);
    break;
  case FULL_COLOR_DITHER:
    if (matched_depth >= 24)
#endif

      Color32DitherImage(l, Cr, Cb, disp, h, w);

#ifndef DISABLE_DITHER
    else
      Color16DitherImage(l, Cr, Cb, disp, h, w);
    break;


#ifdef ENABLE_GRAY_DITHER
  case GRAY_DITHER:
  case GRAY256_DITHER:
    if (matched_depth == 8) 
      GrayDitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 16) 
      Gray16DitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 32 || matched_depth == 24)
      Gray32DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case GRAY2_DITHER:
  case GRAY2562_DITHER:
    if (matched_depth == 8) 
      Gray2DitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 16) 
      Gray216DitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 32 || matched_depth == 24)
      Gray232DitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif

  case NO_DITHER:
    break;


#ifdef ENABLE_PPM_DITHER
  case PPM_DITHER:
    Color32DitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif

  case ORDERED_DITHER:
    OrderedDitherImage(l, Cr, Cb, disp, h, w);
    break;


#ifdef ENABLE_MONO_DITHER
  case MONO_DITHER:
    MonoDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MONO_THRESHOLD:
    MonoThresholdImage(l, Cr, Cb, disp, h, w);
    break;
#endif


#ifdef ENABLE_ORDERED2_DITHER
  case ORDERED2_DITHER:
    Ordered2DitherImage(l, Cr, Cb, disp, h, w);
    break;
#endif


#ifdef ENABLE_MB_ORDERED_DITHER
  case MBORDERED_DITHER:
    MBOrderedDitherImage(l, Cr, Cb, disp, h, w, vid_stream->ditherFlags);
    break;
#endif
  }
#endif
}
