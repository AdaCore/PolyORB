
#include "imageMpegLocal.H"




void imageMpegLocal_impl::putFrame(const mpeg::mpegFrame f)
{
	decoder->putFrame(f);
}


void imageMpegDecoder::putFrame(const mpeg::mpegFrame f)
{
        localBuff->writeBuffer((char*)f,mpeg::frameSize);
}




imageMpegLocal_impl::imageMpegLocal_impl(const char* imageFileName)
{

	decoder= new imageMpegDecoder(imageFileName);
	if(decoder==NULL)
		PANIC(FALSE,"Pb alloc decoder\n");
}


imageMpegDecoder::imageMpegDecoder(const char* imageFileName)
{





 	initialRead=TRUE;


  	localBuff= new c_vProtecBuffer<char>(mpeg::frameSize*10);
  	if(localBuff==NULL)
		PANIC(FALSE,"Pb alloc localBuff\n");


//
// Initilialisation des attributrs
//
	noDisplayFlag = 0;
	loopFlag = 0;

#ifdef DCPREC
	dcprec=0;
#endif

	framerate=0;
	shmemFlag = 0;

#ifdef QUIET
	quietFlag = 1;
#else
	quietFlag = 0;
#endif


	partialFlag = 0;
 	startFrame = -1;
 	endFrame = -1;
	gammaCorrectFlag = 0;
	gammaCorrect = 1.0;
	chromaCorrectFlag = 0;
	chromaCorrect = 1.0;
#ifdef QUALITY
	qualityFlag = 1;
#else
	qualityFlag = 0;
#endif

	numInput=0;

        rate_deal = 0;
        TotalFrameCount= 0;
        ControlShow     = CTRLBAR_ON;


#ifndef NOCONTROLS
        LastState = CTRL_UNDEFINED;
        state = STOPWATCH_STOP;
        ControlState    = CTRL_UNDEFINED;
        ControlMotion   = CTRLMOTION_OFF;

        CtrlBarHeight =  31;
        CtrlBarBorder =   4;
        ChildBorder   =   2;
        ChildMargin   =   6;

        ctrlwindow = 0;
        screen = -1;
        ctrlfont = NULL;
        ctrl_init = 0;

	protocol_atom = (Atom)None;
	delete_atom   = (Atom)None;

	ChildCount    =   7;
#endif


	// 16bits.c
	//
	b16_L_tab=NULL;
	b16_Cr_r_tab=NULL;
	b16_Cr_g_tab=NULL;
	b16_Cb_g_tab=NULL,
        b16_Cb_b_tab=NULL;
	r_2_pix=NULL;
	g_2_pix=NULL;
	b_2_pix=NULL;
	r_2_pix_alloc=NULL;
	g_2_pix_alloc=NULL;
	b_2_pix_alloc=NULL;


	// mono.c
	//        
#ifdef ENABLE_MONO_DITHER
	curr = NULL;
        next = NULL;
#endif


	// video.c
	//


	B_frameRegulator=0;
	P_frameRegulator=0;



#ifdef ANALYSIS
        bitCount = 0;
        showmb_flag = 0;
        showEachFlag = 0;
#endif


	//
	// Debut du main
	//

  	int i;

	firstStream=-1;
	workToDo=TRUE;
  	doDisplay=0;
	seekValue=0;
	owncmFlag=0; 
#ifdef ENABLE_PPM_DITHER
	ppm_width = -1;
	ppm_height = -1;
	ppm_modulus = -1;
        munged = 0;
#endif
	mark = 1;



  	input = (FILE **) malloc(NUMMOVIES*sizeof(FILE *));
  	inputName = (char **) malloc(NUMMOVIES *sizeof(char *));
  	theStream = (VidStream **) malloc(NUMMOVIES *sizeof(VidStream *));
  	curVidStream = (VidStream **) malloc(NUMMOVIES *sizeof(VidStream *));
	if( 	(input==NULL) ||
		(inputName==NULL) ||	
		(theStream==NULL) ||	
		(curVidStream==NULL)   )
		PANIC(FALSE,"Pb alloc input\n");



  	for (i = 0; i < NUMMOVIES; i++) 
		{
		input[i] = NULL;
     		inputName[i] = "stdin";
     		theStream[i] = NULL;
     		curVidStream[i] = NULL;
     		xinfo[i].hints.x = -1;
     		xinfo[i].hints.y = -1;
     		xinfo[i].ExistingWindow = 0;
  	}
  	name = (char *) "";


#ifndef DISABLE_DITHER
#ifndef DEFAULT_ORDERED_DITHER
  	xinfo[0].ditherType = FULL_COLOR_DITHER;
#else
  	xinfo[0].ditherType = ORDERED_DITHER;
#endif
#endif

	xinfo[0].ditherType=ORDERED_DITHER;



  	noDisplayFlag = 0;

#ifdef SH_MEM
  	shmemFlag = 1;
#endif



  	lum_values = (int *) malloc(LUM_RANGE*sizeof(int));
  	cr_values = (int *) malloc(CR_RANGE*sizeof(int));
  	cb_values = (int *) malloc(CB_RANGE*sizeof(int));
	if( 	(lum_values==NULL) ||
		(cr_values==NULL) ||	
		(cb_values==NULL)   )
		PANIC(FALSE,"Pb alloc lum_values\n");




//
//   Inititialisation comme si on avait ouvert le ficher !!!
//  
  	inputName[numInput]=(char*)imageFileName;
  	input[numInput]=NULL;
  	numInput++;

  
  	init_tables();
  	for (i = 0;  i < numInput;  i++) 
		{
    		xinfo[i].owncmFlag = owncmFlag;
		/* xinfo.ximage is set to null later */
    		xinfo[i].display = NULL;       
    		if (xinfo[i].hints.x == -1) 
			{
    			xinfo[i].hints.x = 200;
      			xinfo[i].hints.y = 300;
    			}
    		xinfo[i].hints.width = 150;
    		xinfo[i].hints.height = 150;
    		xinfo[i].visual = NULL;
    		xinfo[i].name = inputName[i];
    		xinfo[i].cmap = 0;
    		xinfo[i].gc = 0;
  		}

#ifndef DISABLE_DITHER
  if (xinfo[0].ditherType == MONO_DITHER ||
      xinfo[0].ditherType == MONO_THRESHOLD)
    xinfo[0].depth= 1;

  switch (xinfo[0].ditherType) {
    

#ifdef ENABLE_HYBRID_DITHER
  case HYBRID_DITHER:
    InitColor();
    InitHybridDither();
    InitDisplay(name, &xinfo[0]);
    break;
#endif
    
#ifdef ENABLE_HYBRIDERR_DITHER
  case HYBRID2_DITHER:
    InitColor();
    InitHybridErrorDither();
    InitDisplay(name, &xinfo[0]);
    break;
#endif
    
#ifdef ENABLE_FS4_DITHER
  case FS4_DITHER:
    InitColor();
    InitFS4Dither();
      InitDisplay(name, &xinfo[0]);
    break;
#endif
    
#ifdef ENABLE_FS2_DITHER
  case FS2_DITHER:
    InitColor();
    InitFS2Dither();
    InitDisplay(name, &xinfo[0]);
    break;
#endif
    
#ifdef ENABLE_FS2FAST_DITHER
  case FS2FAST_DITHER:
    InitColor();
    InitFS2FastDither();
    InitDisplay(name, &xinfo[0]);
    break;
#endif
    
#ifdef ENABLE_2x2_DITHER
  case Twox2_DITHER:
    InitColor();
    Init2x2Dither();
    InitDisplay(name, &xinfo[0]);
    PostInit2x2Dither();
    break;
#endif

#ifdef ENABLE_GRAY_DITHER
  case GRAY_DITHER:
  case GRAY2_DITHER:
    InitGrayDisplay(name, &xinfo[0]);
    break;

  case GRAY256_DITHER:
  case GRAY2562_DITHER:
    InitGray256Display(name, &xinfo[0]);
    break;
#endif

  case FULL_COLOR_DITHER:
  case FULL_COLOR2_DITHER:
    InitColorDisplay(name, &xinfo[0]);
    InitColorDither(xinfo[0].depth>=24);
#else
    InitColorDisplay(name, &xinfo[0]);
    InitColorDither(xinfo[0].depth>=24);
#endif
#ifndef DISABLE_DITHER
    break;

  case NO_DITHER:
    shmemFlag = 0;
    break;

  case PPM_DITHER:
    shmemFlag = 0;
    wpixel[0] = 0xff;
    wpixel[1] = 0xff00;
    wpixel[2] = 0xff0000;
    xinfo[0].depth = 24;
    InitColorDither(1);
    break;

  case ORDERED_DITHER:
    InitColor();
    InitOrderedDither();
    InitDisplay(name, &xinfo[0]);
    break;

#ifdef ENABLE_MONO_DITHER
  case MONO_DITHER:
  case MONO_THRESHOLD:
    InitMonoDisplay(name, &xinfo[0]);
    break;
#endif

#ifdef ENABLE_ORDERED2_DITHER
  case ORDERED2_DITHER:
    InitColor();
    InitDisplay(name, &xinfo[0]);
    InitOrdered2Dither();
    break;
#endif

#ifdef ENABLE_MB_ORDERED_DITHER
  case MBORDERED_DITHER:
    InitColor();
    InitDisplay(name, &xinfo[0]);
    InitMBOrderedDither();
    break;
#endif
  }
#endif

#ifdef SH_MEM
    if (shmemFlag && (xinfo[0].display != NULL)) {
      if (!XShmQueryExtension(xinfo[0].display)) {
        shmemFlag = 0;
        if (!quietFlag) {
          fprintf(stderr, "Shared memory not supported\n");
          fprintf(stderr, "Reverting to normal Xlib.\n");
        }
      }
    }
#endif



};





CORBA::Long imageMpegLocal_impl::displayFrame( )
{
	return (decoder->displayFrame());
}


CORBA::Long imageMpegDecoder::displayFrame( )
{
    int i,y;


 if(initialRead==TRUE)
	{
	initialRead=FALSE;

  InitCrop();

  y=300;
  largy=0;
  for (i=0;i<numInput;i++) {
    doDisplay=!noDisplayFlag;
#ifndef DISABLE_DITHER
    if ((xinfo[i].ditherType == NO_DITHER) ||
        (xinfo[i].ditherType == PPM_DITHER))
       doDisplay = FALSE;
#endif
    lastStream = i-1;
    while ((lastStream>=0) && (theStream[lastStream]==NULL)) {
       lastStream--;
    }
    if ((i != 0) && doDisplay) {
       if (lastStream > -1) {
         xinfo[i].hints.x =
            xinfo[lastStream].hints.x+10 + theStream[lastStream]->h_size;
         if (theStream[lastStream]->v_size>largy)
	   largy = theStream[lastStream]->v_size;
         if (xinfo[i].hints.x > DisplayWidth(xinfo[firstStream].display,
			       XDefaultScreen(xinfo[firstStream].display)) -80) {

		y += largy + 30;
		largy = 0;
		xinfo[i].hints.x = 0;
         }
         xinfo[i].hints.y = y;
         xinfo[i].visual = xinfo[firstStream].visual;
         xinfo[i].cmap = xinfo[firstStream].cmap;
         xinfo[i].gc = xinfo[firstStream].gc;
       }
       xinfo[i].display = xinfo[0].display;
       xinfo[i].depth = xinfo[0].depth;
       xinfo[i].ditherType = xinfo[0].ditherType;
       InitColorDisplay(name, &xinfo[i]);
    }
    curVidStream[i] = theStream[i] = NewVidStream((unsigned int) BUF_LENGTH);

#ifdef ENABLE_PPM_DITHER
    theStream[i]->ppm_width = ppm_width;
    theStream[i]->ppm_height = ppm_height;
    theStream[i]->ppm_modulus = ppm_modulus;
#endif
    theStream[i]->input = input[i];
    theStream[i]->seekValue = seekValue;
    theStream[i]->filename = inputName[i];

    theStream[i]->I_number=0;
    theStream[i]->B_number=0;
    theStream[i]->P_number=0;
    theStream[i]->I_discard=0;
    theStream[i]->B_discard=0;
    theStream[i]->P_discard=0;
    theStream[i]->D_discard=0;

    theStream[i]->ditherType = xinfo[i].ditherType;
    theStream[i]->matched_depth = xinfo[i].depth;
    mark = quietFlag;


    quietFlag=1;
    if (mpegVidRsrc(0, theStream[i], 1, &xinfo[i])==NULL) {
       if (doDisplay) {
         XDestroyWindow(xinfo[i].display, xinfo[i].window);
       }	 
       /* stream has already been destroyed */
       curVidStream[i] = theStream[i]=NULL;
       fprintf(stderr, "Skipping movie %d, \"%s\" - not an MPEG stream\n",
	  i, inputName[i]);
       fclose(input[i]);
       if (i+1 == numInput) numInput--;
    } else if (firstStream == -1) firstStream=i;
    quietFlag = mark;

#ifndef DISABLE_DITHER
    if (IS_2x2_DITHER(xinfo[i].ditherType)) {
      mult = 2;
    }
    else {
      mult = 1;  
    }
#else
    mult = 1;
#endif

    if (doDisplay && (theStream[i]!=NULL)) {
      ResizeDisplay((unsigned int) theStream[i]->h_size* mult,
                    (unsigned int) theStream[i]->v_size* mult,
		  &xinfo[i]);
    }
  }

  if (numInput > 1) {
    loopFlag = TRUE;
    framerate = 0;
  }

#ifndef NOCONTROLS
  if (xinfo[0].display == NULL) {
    ControlShow = CTRLBAR_NONE;  /* no display => no controls */
  }

  if (ControlShow != CTRLBAR_NONE) {
    MakeControlBar(&xinfo[0]);
    ControlBar(theStream, xinfo, numInput);
  }
  for (i = 0; i < numInput; i++) {
     if (theStream[i] != NULL) theStream[i]->realTimeStart = ReadSysClock();
  }
#else
  /* Start time for each movie - do after windows are mapped */
  for (i = 0; i < numInput; i++) {
     if (theStream[i] != NULL) theStream[i]->realTimeStart = ReadSysClock();
  }
#endif


#ifndef NOCONTROLS
  if (ControlShow == CTRLBAR_NONE) {
    while (TRUE) {
      for (i=0;i < numInput; i++) {
        while (theStream[i]->film_has_ended != TRUE) {
          mpegVidRsrc(0, theStream[i], 0, &xinfo[i]);
        }
        if (loopFlag) {
          rewind(theStream[i]->input); 
          ResetVidStream(theStream[i]); /* Reinitialize vid_stream pointers */
          if (theStream[i]->seekValue < 0) {
            theStream[i]->seekValue = 0 - theStream[i]->seekValue;
          }
          mpegVidRsrc(0, theStream[i], 1, &xinfo[i]); /* Process start codes */
        } else if (doDisplay) break;
        else goto done;
      }
     }
   }
  else {
    ControlLoop(theStream, xinfo, numInput);
  }

done:
  mark=0;
  for (i=0;i < numInput; i++) { 
    DestroyVidStream(theStream[i], &xinfo[i]);
    if ((xinfo[i].display != NULL) && !mark) {
      XCloseDisplay(xinfo[i].display);
      mark=1;
    }
  }
  exit(0);
#else /* !NOCONTROLS */
  if (!numInput) 
     PANIC(FALSE,"Must enter MPEG file to play\n");
#endif /* NOCONTROLS */
 

}






     workToDo = FALSE;
     for (i = 0; i < numInput; i++) {
       if (theStream[i] != NULL) {
         mark = theStream[i]->totNumFrames;
         /* make sure we do a whole frame */
         while (mark == theStream[i]->totNumFrames) {
             mpegVidRsrc(0, theStream[i], 0, &xinfo[i]);
         }


         if (theStream[i]->film_has_ended) {
           if (loopFlag) {
             clear_data_stream(theStream[i]);
             /* Reinitialize vid_stream pointers */
             ResetVidStream(theStream[i]);
             rewind(theStream[i]->input);
             if (theStream[i]->seekValue < 0) {
               theStream[i]->seekValue = 0 - theStream[i]->seekValue;
             }
#ifdef ANALYSIS 
             init_stats();
#endif
             /* Process start codes */
             if (mpegVidRsrc(0, theStream[i], 1, &xinfo[i])==NULL) {
	       /* print something sensible here,
		  but we only get here if the file is changed while we
		  are decoding, right?
		*/
	     }
           } /* loopFlag */
	 }   /* film_has_ended */
         workToDo = workToDo || (!theStream[i]->film_has_ended);
       } /* theStream[i]!=NULL */
     }   /* for (i.. */


     return workToDo; 

}



