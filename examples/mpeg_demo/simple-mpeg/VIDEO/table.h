


#ifndef VIDEO_TABLE_H
#define VIDEO_TABLE_H


#include <stdio.h>
#include <assert.h>
#include "../imageMpegLocal.H"
#include "decoders.h"
#include "util.h" 
#include "video.h"



	extern coded_block_pattern_entry coded_block_pattern[512];
	extern dct_dc_size_entry dct_dc_size_luminance[32];
	extern dct_dc_size_entry dct_dc_size_luminance1[16];
	extern dct_dc_size_entry dct_dc_size_chrominance[32];
	extern dct_dc_size_entry dct_dc_size_chrominance1[32];
	extern unsigned short int dct_coeff_tbl_0[256];
	extern unsigned short int dct_coeff_tbl_1[16]; 
	extern unsigned short int dct_coeff_tbl_2[4];
	extern unsigned short int dct_coeff_tbl_3[4]; 
	extern unsigned short int dct_coeff_next[256]; 
	extern unsigned short int dct_coeff_first[256]; 
 


	extern unsigned int nBitMask[];
	extern unsigned int bitMask[];
	extern unsigned int rBitMask[];
	extern unsigned int bitTest[];
	extern int zigzag_direct[64];
	extern int quantTbl[4];



#endif
