

#ifndef MPEG_IMAGE_DECODER_H
#define MPEG_IMAGE_DECODER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <iomanip.h>
#include <math.h>
#include <sys/types.h>
#include <signal.h>
#include <netinet/in.h>

#include "dominante.hh"

#include "varBuffer.H"


#include "VIDEO/video.h"
#include "VIDEO/util.h"
#include "VIDEO/dither.h"
#include "VIDEO/ctrlbar.h"
#include "VIDEO/decoders.h"
#include "VIDEO/fs2.h"
#include "VIDEO/fs4.h"



#define DEFAULT_ORDERED_DITHER
#define BUF_LENGTH 8000
#define DITH_SIZE 16
#define NUMMOVIES 1
#define INITIAL_STATE CTRL_PLAY
#define L_DITH_RANGE (((LUM_RANGE-1)*4)+1)
#define CR_DITH_RANGE (((CR_RANGE-1)*4)+1)
#define CB_DITH_RANGE (((CB_RANGE-1)*4)+1)
#define RAND_ERR_SUBVAL 3
#define RAND_ERR_RANGE 7
#define LUM_RANGE 8
#define CR_RANGE  4
#define CB_RANGE  4




class imageMpegDecoder
{

public:

        imageMpegDecoder(const char* name);
        CORBA::Long displayFrame();
        void putFrame(const mpeg::mpegFrame f);




        // buffer local
        c_vProtecBuffer<char>* localBuff;




//
// Methodes du decodeur 
//

/* video.c */
	void ReconIMBlock(VidStream *vid_stream, int bnum);
	void ReconPMBlock(VidStream *vid_stream, int bnum, int recon_right_for, int recon_down_for,int  zflag);
	void ReconBMBlock(VidStream *vid_stream, int bnum, int recon_right_back,int  recon_down_back,int  zflag);
	void ReconBiMBlock(VidStream *vid_stream, int bnum, int recon_right_for, int recon_down_for, int recon_right_back, int recon_down_back, int zflag);
	void ReconSkippedBlock(unsigned char *source, unsigned char *dest, int row, int col, int row_size, int right, int down, int right_half, int down_half, int width);
	void DoPictureDisplay(VidStream *vid_stream, XInfo *xinfo);
	int ParseSeqHead(VidStream *vid_stream, XInfo *xinfo);
	int ParseGOP(VidStream *vid_stream);
	int ParsePicture(VidStream *vid_stream, TimeStamp time_stamp);
	int ParseSlice(VidStream *vid_stream);
	int ParseMacroBlock(VidStream *vid_stream);
	void ProcessSkippedPFrameMBlocks(VidStream *vid_stream);
	void ProcessSkippedBFrameMBlocks(VidStream *vid_stream);


/* util.c */
	void correct_underflow (VidStream *vid_stream );
	int next_bits (int num , unsigned int mask , VidStream *vid_stream );
	char *get_ext_data (VidStream *vid_stream );
	int next_start_code (VidStream *vid_stream);
	char *get_extra_bit_info (VidStream *vid_stream );

/* video.c */
#ifdef ANALYSIS
	void init_stats (void );
	void PrintAllStats (VidStream *vid_stream );
	void EndTime(void);
	void StartTime(void);
	void init_stat_struct( Statval *astat);
	void PrintOneStat(void);	
	void CollectStats(void);	
#endif

	double ReadSysClock (void );
	void PrintTimeInfo ( VidStream *vid_stream );
	void InitCrop (void );
	VidStream *NewVidStream (unsigned int buffer_len );
	void ResetVidStream (VidStream *vid );
	void DestroyVidStream (VidStream *astream, XInfo *xinfo );
	PictImage *NewPictImage ( VidStream *vid_stream, XInfo *xinfo );
	void DestroyPictImage (PictImage *apictimage, XInfo *xinfo );
	VidStream *mpegVidRsrc (TimeStamp time_stamp,VidStream *vid_stream, int first , XInfo *xinfo);

/* parseblock.c */
	void ParseReconBlock (int n, VidStream *vid_stream );
	void ParseAwayBlock (int n , VidStream *vid_stream );

/* motionvector.c */
	void ComputeForwVector (int *recon_right_for_ptr , int *recon_down_for_ptr , VidStream *the_stream );
	void ComputeBackVector (int *recon_right_back_ptr , int *recon_down_back_ptr, VidStream *the_stream );

/* decoders.c */
	void init_tables (void );
	void init_mb_addr_inc(void);
	void init_mb_type_P(void);
	void init_mb_type_B(void);
	void init_motion_vectors(void);
	void decodeDCTDCSizeLum (unsigned int *value );
	void decodeDCTDCSizeChrom (unsigned int *value );
	void decodeDCTCoeffFirst (unsigned int *run , int *level );
	void decodeDCTCoeffNext (unsigned int *run , int *level );

/* from main.c or equivalent */
	void DoDitherImage ( VidStream *vid_stream );
	void getposition( char *arg, int *xpos, int *ypos);


/* gdith.c */
	void InitColor (void );

#ifdef SH_MEM
	int HandleXError (Display *dpy , XErrorEvent *event );
	void InstallXErrorHandler ( Display *display );
	void DeInstallXErrorHandler ( Display *display );
#endif

	void ResizeDisplay (unsigned int w , unsigned int h, XInfo *xinfo );
	void InitDisplay (char *name , XInfo *xinfo );
	void InitColorDisplay (char *name , XInfo *xinfo );
	int  MakeWindow(char* name,XInfo*  xinfo);
	void ConvertColor( unsigned int l,unsigned int  cr,unsigned int  cb, unsigned char *r, unsigned char *g,unsigned char *b);


#ifndef NOCONTROLS
	void ExecuteDisplay (VidStream *vid_stream , int frame_increment , XInfo *xinfo);
#else
	void ExecuteDisplay (VidStream *vid_stream, XInfo *xinfo );
#endif

#ifdef ENABLE_PPM_DITHER
	void ExecutePPM (VidStream *vid_stream );
#endif


#ifdef NO_GETTIMEOFDAY
	struct timeval {long tv_sec, tv_usec;}; /* secs and usecs since 1-jan-1970 */
	int gettimeofday (struct timeval * retval, void * unused);
#endif



#ifdef ENABLE_FS2_DITHER
/* fs2.c */
	void InitFS2Dither (void );
	void FS2DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *disp , int rows , int cols );
#endif



#ifdef ENABLE_FS2FAST_DITHER
/* fs2fast.c */
	void InitFS2FastDither (void );
	void FS2FastDitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
#endif



#ifdef ENABLE_FS4_DITHER
/* fs4.c */
	void InitFS4Dither (void );
	void FS4DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *disp , int rows , int cols );
#endif



#ifdef ENABLE_HYBRID_DITHER
/* hybrid.c */
	void InitHybridDither (void );
	void HybridDitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
#endif


#ifdef ENABLE_HYBRIDERR_DITHER
/* hybriderr.c */
	void InitHybridErrorDither (void );
	void HybridErrorDitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
#endif


#ifdef ENABLE_2x2_DITHER
/* 2x2.c */
	void Init2x2Dither (void );
	void RandInit (int h , int w );
	void PostInit2x2Dither (void );
	void Twox2DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
#endif


#ifdef ENABLE_GRAY_DITHER
/* gray.c */
	void GrayDitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
	void Gray2DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
	void Gray16DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
	void Gray216DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
	void Gray32DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
	void Gray232DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );


/* gdith.c */
	void InitGrayDisplay (char *name, XInfo *xinfo );
	void InitGray256Display (char *name, XInfo *xinfo );
#endif


#ifdef ENABLE_MONO_DITHER
/* mono.c */
	void MonoThresholdImage(unsigned char *lum, unsigned char *cr, unsigned char *cb, unsigned char *out, int h, int w);
	void MonoDitherImage(unsigned char *lum, unsigned char *cr, unsigned char *cb, unsigned char *out, int h, int w);


/* gdith.c */
	void InitMonoDisplay (char *name, XInfo *xinfo );
#endif


/* jrevdct.c */
	void init_pre_idct (void );
	void j_rev_dct_sparse (DCTBLOCK data , int pos );
	void j_rev_dct (DCTBLOCK data );

/* floatdct.c */
#ifdef FLOATDCT
	void init_float_idct (void );
	void float_idct (short* block );
#endif

/* 16bit.c */
	int number_of_bits_set( unsigned long a);
	unsigned long shifted_down( unsigned long a);
	int free_bits_at_top( unsigned long a);
	int free_bits_at_bottom( unsigned long a);
	void InitColorDither (int );
	void Color16DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols );
	void Color32DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols );

/* 16bit2x2.c */
	void Twox2Color16DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols );
	void Twox2Color32DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols );

/* util32.c */
	Visual *FindFullColorVisual (Display *dpy , int *depth );
	void CreateFullColorWindow ( XInfo *xinfo);

/* ordered.c */
	void InitOrderedDither (void );
	void OrderedDitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );


#ifdef ENABLE_ORDERED2_DITHER
/* ordered2.c */
	void InitOrdered2Dither (void );
	void Ordered2DitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w );
#endif


#ifdef ENABLE_MB_ORDERED_DITHER
/* mb_ordered.c */
	void InitMBOrderedDither (void );
	void MBOrderedDitherImage (unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w, char *ditherFlags );
	void MBOrderedDitherDisplayCopy (VidStream *vid_stream , int mb_addr , int motion_forw , int r_right_forw , int r_down_forw , int motion_back , int r_right_back , int r_down_back , unsigned char *past , unsigned char *future );
#endif


/* readfile.c */
	void SeekStream (VidStream *vid_stream );
	void clear_data_stream ( VidStream *vid_stream);
	int get_more_data ( VidStream *vid_stream );
	int pure_get_more_data (unsigned int *buf_start , int max_length , int *length_ptr , unsigned int **buf_ptr, VidStream *vid_stream );
	int read_sys ( VidStream *vid_stream, unsigned int start );
	int ReadStartCode ( unsigned int *startCode, VidStream *vid_stream );
	int ReadPackHeader ( double *systemClockTime, unsigned long *muxRate, VidStream *vid_stream );
	int ReadSystemHeader ( VidStream *vid_stream);
	int find_start_code ( FILE *input );
	int ReadPacket ( unsigned char packetID, VidStream *vid_stream );
	void ReadTimeStamp ( unsigned char *inputBuffer, unsigned char *hiBit, unsigned long *low4Bytes);
	void ReadSTD ( unsigned char *inputBuffer, unsigned char *stdBufferScale, unsigned long *stdBufferSize);
	void ReadRate ( unsigned char *inputBuffer, unsigned long *rate);
	int MakeFloatClockTime ( unsigned char hiBit, unsigned long low4Bytes, double *floatClockTime);


#ifndef NOCONTROLS
/* ctrlbar.c */
	double StopWatch (int action );
	Bool WindowMapped (Display *dsp, XEvent *xev, char *window );
	Bool IfEventType (Display *dsp, XEvent *xev, char *type );
	void MakeControlBar ( XInfo *xinfo );
	void UpdateFrameTotal (Display *display);
	void UpdateControlDisplay (Display *display);
	void ControlBar (VidStream **vid_stream, XInfo *xinfo, int numMovies );
	void ControlLoop (VidStream **theStream, XInfo *xinfo, int numStreams );
#endif /* !NOCONTROLS */


//
// Attributs du decodeur
// 



#ifdef DCPREC
	/* Declaration of global variable to hold DC precision */
	int dcprec ;
#endif

	/* Global file pointer to incoming data. */
	FILE **input;
	char **inputName;


	int first;


	int ControlShow     ;         /* ON => Show (display) control bar */
	long TotalFrameCount; /* Total number of frames processed, including loops */


#ifndef NOCONTROLS
	/* Time */
	struct timeval start,current;  /* Working times */
	struct timeval elapsed;        /* Previously accumulated time */


	int ControlState    ;     /* Current control state */
	int ControlMotion   ;     /* Pause mode */


	/* File statics */
	int CtrlBarWidth;
	int CtrlBarHeight ;
	int CtrlBarBorder ;
	int ChildBorder   ;
	int ChildMargin   ;

	int LastState;         
	XEvent event;
	Window ctrlwindow ;
	int screen ;
	XFontStruct* ctrlfont ;
	int fontheight, fontwidth;
	unsigned long fgcolor, bgcolor;

	int ctrl_init ;  /* 1 => Control windows have been initialized */

	/* Support for WM_DELETE_WINDOW */
	Atom protocol_atom;
	Atom delete_atom ;

	/* Child windows */
	int ChildCount;
	Window frametotalwin;
	int ftw_width, ftw_height;
	Window rewindwin;
	int rww_width, rww_height;
	Window pausewin;
	int psw_width, psw_height;
	Window playwin;
	int plw_width, plw_height;

	Window stepwin;
	int stw_width, stw_height;
	Window exitwin;
	int exw_width, exw_height;
	Window loopwin;
	int lpw_width, lpw_height;

	int state;

#endif


	VidStream **theStream;


	int rate_deal;
	int one_frame_time;
	struct timeval tftarget, tfnow;


	/* Loop flag. */
	int loopFlag;

	/* Shared memory flag. */
	int shmemFlag;

	/* Quiet flag. */
	int quietFlag;


	/* Display image on screen? */
	int noDisplayFlag;

	/* Framerate, -1: specified in stream (default)
               0: as fast as possible
               N (N>0): N frames/sec  
               */
	int framerate;

	/* Flags/values to control Arbitrary start/stop frames. */
	int partialFlag, startFrame, endFrame;

	/* Flag for gamma correction */
	int gammaCorrectFlag;
	double gammaCorrect;

	/* Flag for chroma correction */
	int chromaCorrectFlag;
	double chromaCorrect;

	/* Flag for high quality at the expense of speed */
	int qualityFlag;

	/* global variable for interrupt handlers */
	VidStream **curVidStream;

	/* Brown - put X specific variables in xinfo struct */
	XInfo xinfo[NUMMOVIES];
	int numInput;


//
// Attributs 16bits.c
//
	int *b16_L_tab, *b16_Cr_r_tab, *b16_Cr_g_tab, *b16_Cb_g_tab, *b16_Cb_b_tab;

/*
 * We define tables that convert a color value between -256 and 512
 * into the R, G and B parts of the pixel. The normal range is 0-255.
 */

	long *r_2_pix;
	long *g_2_pix;
	long *b_2_pix;
	long *r_2_pix_alloc;
	long *g_2_pix_alloc;
	long *b_2_pix_alloc;



#ifdef ENABLE_2x2_DITHER
//
// Attributs 2x2.c
//


/* Array containing actual pixel values for each possible 2x2 dither pattern. */

	unsigned char *dith_a;

/* Arrays mapping lum, cr, and cb values to portions of dither pattern code.
   The addtion of one value from each array yields a valid dither pattern
   code.
*/

	int lval_a[256+RAND_ERR_RANGE-1];
	int rval_a[256+RAND_ERR_RANGE-1];
	int bval_a[256+RAND_ERR_RANGE-1];

	/* Range of possible dither patterns in each channel. */


	/* Arrays of random error terms added to break up contours. */

	int *randval_a;
	int **randptr_a;
#endif


//
// Attributs ctrlbar.c
//




//
// Attributs decoders.c
//


/* Decoding table for macroblock_address_increment */
	mb_addr_inc_entry     mb_addr_inc[2048];

/* Decoding table for macroblock_type in predictive-coded pictures */
	mb_type_entry         mb_type_P[64];

/* Decoding table for macroblock_type in bidirectionally-coded pictures */
	mb_type_entry         mb_type_B[64];

/* Decoding table for motion vectors */
	motion_vectors_entry  motion_vectors[2048];



//
// Attributs floatdct.c
//
#ifdef FLOATDCT

/* cosine transform matrix for 8x1 IDCT */
	double c_dct[8][8];

#endif



//
// Attributs fs2.c
//
#ifdef ENABLE_FS2_DITHER

/* Structures for precomputed error propogation values. */

	FS2DithVal fs2_lum_index[256];
	FS2DithVal fs2_cr_index[256];
	FS2DithVal fs2_cb_index[256];

	char *fs2_cur_row_error, *fs2_next_row_error;
#endif


//
// Attributs fs2fast.c
//

#ifdef ENABLE_FS2FAST_DITHER

/* Arrays containing error values for floyd-steinberg dithering. */

	int deltay[256];
	int deltau[256];
	int deltav[256];
	int deltay2[256];
	int deltau2[256];
	int deltav2[256];

	int *yerr1;
	int *yerr2;
	int *uerr1;
	int *uerr2;
	int *verr1;
	int *verr2;
#endif


//
// Attributs fs4.c
//


#ifdef ENABLE_FS4_DITHER
/* Structures containing precomputed error terms. */

	FS4Dither fs4_lum_index[256];
	FS4Dither fs4_cr_index[256];
	FS4Dither fs4_cb_index[256];

	char *fs4_cur_row_error, *fs4_next_row_error;

#endif

//
// Attributs gdith.c
//


/* Array that remaps color numbers to actual pixel values used by X server. */

	unsigned char pixel[256];
	unsigned long wpixel[256];

/* Arrays holding quantized value ranged for lum, cr, and cb. */

	int *lum_values;
	int *cr_values;
	int *cb_values;

/* Luminance and chrominance lookup tables */
	double *gd_L_tab, *gd_Cr_r_tab, *gd_Cr_g_tab, *gd_Cb_g_tab, *gd_Cb_b_tab;



//
// Attributs hybrid.c
//

#ifdef ENABLE_HYBRID_DITHER

/* Structures used to implement hybrid ordered dither/floyd-steinberg
   dither algorithm.
*/

	unsigned char *h_l_darrays[DITH_SIZE];
	unsigned char h_cr_fsarray[256][4];
	unsigned char h_cb_fsarray[256][4];
#endif


//
// Attributs hybriderr.c
//

#ifdef ENABLE_HYBRIDERR_DITHER


/* Structures used for hybrid dither with errors propogated. */

	unsigned char *he_l_darrays[DITH_SIZE];
	unsigned char *l_darrays0, *l_darrays1, *l_darrays2, *l_darrays3;
	unsigned char *l_darrays4, *l_darrays5, *l_darrays6, *l_darrays7;
	unsigned char *l_darrays8, *l_darrays9, *l_darrays10, *l_darrays11;
	unsigned char *l_darrays12, *l_darrays13, *l_darrays14, *l_darrays15;
	unsigned char he_cr_fsarray[256*256][4];
	unsigned char he_cb_fsarray[256*256][4];
	unsigned short  c_fserr[256*256][2];
	int *he_cr_row_errs;
	int *he_cb_row_errs;


#endif

//
// Attributs jrevdct.c
//

/* Precomputed idct value arrays. */

	DCTELEM PreIDCT[64][64];



//
// Attributs mb_ordered.c
//

#ifdef ENABLE_MB_ORDERED_DITHER

/* Structures used to implement macroblock ordered
   dither algorithm.
*/

	unsigned char ***mbo_ditherPtr[DITH_SIZE];


	int    *curr;
	int    *next;
#endif


//
// Attributs ordered.c
//


/* Structures used to implement hybrid ordered dither/floyd-steinberg
   dither algorithm.
*/

	unsigned char *l_darrays[DITH_SIZE];
	unsigned char *cr_darrays[DITH_SIZE];
	unsigned char *cb_darrays[DITH_SIZE];


//
// Attributs ordered2.c
//

#ifdef ENABLE_ORDERED2_DITHER

/* Structures used to implement hybrid ordered dither/floyd-steinberg
   dither algorithm.
*/

	unsigned char ***o_ditherPtr[DITH_SIZE];

#endif

//
// Attributs readfile.c
//


#ifdef ANALYSIS
/* Statistics */

	int numPacks ;
	int numPackets;
	int numSystemHeaders ;

	int gNumAudioPackets;
	int gNumVideoPackets;
	int gNumPaddingPackets;
	int gNumReservedPackets;
	int gNumPrivate_1_Packets;
	int gNumPrivate_2_Packets;
#endif

/* Initialize P and B skip flags. */

	int No_P_Flag ;
	int No_B_Flag ;
	int No_I_Flag ;
	int No_D_Flag ;

	int P_ratio;
	int B_ratio;
	int I_ratio;
	int D_ratio;


/*	Regulateur de ratio */
	int B_frameRegulator;
	int P_frameRegulator;



	unsigned char cropTbl[NUM_CROP_ENTRIES];



#ifdef ANALYSIS
	int showmb_flag ;
	int showEachFlag ;
	unsigned int bitCount ;
	Statval stat_a[4];
	unsigned int pictureSizeCount;
	unsigned int mbSizeCount;
	unsigned int *mbCBPPtr, *mbCoeffPtr, *mbSizePtr;
	unsigned int cacheHit[8][8];
	unsigned int cacheMiss[8][8];

#endif


//
//  main.c
//
  	char *name;
  	int mark;
        long seekValue;/* holds value before it is put in vid_stream */
        int  owncmFlag;
        int doDisplay; /* Current movie is displaying on screen */
        int mult, largy, lastStream, firstStream, workToDo;


#ifdef ENABLE_PPM_DITHER
  	int ppm_width,  ppm_height, ppm_modulus;
	int munged;
	char mungedInputName[300];
#endif



	// Permet de determiner si toutes les initialisations
	// ont ete effectues
	//
	int initialRead;


};


class imageMpegLocal_impl : public virtual mpeg::_sk_mpegDecoder
{


public:
  	imageMpegLocal_impl(const char* name);
  	virtual CORBA::Long displayFrame();
  	virtual void        putFrame(const mpeg::mpegFrame f);




private:

	imageMpegDecoder* decoder;
};


#endif

