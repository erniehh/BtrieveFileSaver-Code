/* 

	Copyright (C)2010 dbcoretech ltd. <BtrieveFileSaver@dbcoretech.com>

	This file is part of BtrieveFileSaver.

    BtrieveFileSaver is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BtrieveFileSaver is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BtrieveFileSaver.  If not, see <http://www.gnu.org/licenses/>.

*/
#if defined(_WIN32)
#define _CRT_SECURE_NO_WARNINGS	/* Disable deprecation warning in VS2005 */
#endif /* _WIN32 */
 
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <conio.h>


#ifdef _WIN32
#include <windows.h>
#endif /* _WIN32 */

#include "BtrieveFileSaverLib.h"

#if defined(DEBUG)
#define	DEBUG_TRACE(x) do {printf x; putchar('\n'); fflush(stdout);} while (0)
#else
#define DEBUG_TRACE(x)
#endif /* DEBUG */

/*
 * Error codes for all functions that return 'int'.
 */
enum error_t {
	BFS_ERROR,
	BFS_SUCCESS,
	BFS_NOT_FOUND,
	BFS_BUFFER_TOO_SMALL
};

struct option {
	const char	*name;
	const char	*description;
	const char	*default_value;
	int			index;
	enum		error_t (*setter) (const char*);
};

/*
* Prototyping
*/
static enum error_t set_input_file	(const char*);
static enum error_t set_output_file	(const char*);
static enum error_t set_format		(const char*);
static enum error_t set_silent_mode (const char*);

/*
 * Numeric indexes for the option values 
 */
enum mg_option_index {
	OPT_INPUT, OPT_OUTPUT, OPT_FORMAT, OPT_SILENT, NUM_OPTIONS
};

static const struct option known_options[] = {
	{"btrin",  "\tfile to extract", NULL, OPT_INPUT, set_input_file},
	{"btrout", "file to save data to", "btrfsout.log", OPT_OUTPUT, set_output_file},
	{"format", "output format\n1 - BUTIL save format\n2 - BUTIL format without len\n3 - HEX dump including leading CRC\n4 - pur HEX dump\n", "1", OPT_FORMAT, set_format},
	{"silent", "silent mode true or false", FALSE, OPT_SILENT, set_silent_mode},
};

#define	BTRIEVE_FILE_SAVER_VERSION	"0.1"
#define	MAX_REC_BUFFER_SIZE			64000	// data container size
#define	MAX_FILE_BUFFER_SIZE		1024	// file I/O buffer
#define	DEBUG_MGS_PREFIX			"*** Btrieve File Saver debug *** "

/*
 * Visual Studio 6 does not know __func__ or __FUNCTION__
 * The rest of MS compilers use __FUNCTION__, not C99 __func__
 * Also use _strtoui64 on modern M$ compilers
 */
#if defined(_MSC_VER) && _MSC_VER < 1300
#define	__func__		"line " STR(__LINE__)
#else
#define	__func__		__FUNCTION__
#endif /* _MSC_VER */

/* global variables */
char				inFileName[_MAX_PATH]	= {0x00};
char				outFileName[_MAX_PATH]	= {0x00};
char				format					= 0x00;
char				*fBuffer				= NULL;
char				silent_mode				= FALSE;
unsigned long int	crc32_table[256];
FILE				*outFile				= NULL;

static void signal_handler(int sig_num)
{
#if !defined(_WIN32)
	if (sig_num == SIGCHLD) {
		do {
		} while (waitpid(-1, &sig_num, WNOHANG) > 0);
	} else
#else /* !_WIN32 */
	exit(EXIT_FAILURE);
#endif 
}

const char * version(void)
{
	return ("\"" BTRIEVE_FILE_SAVER_VERSION );
}

/************************************************************
*	CRC cylcic redundancy check area
*/
unsigned long int reflect(unsigned long int ref, char ch)
{
	unsigned long int value = 0L;
	unsigned long int i		= 1;

	// Swap bit 0 for bit 7
	// bit 1 for bit 6, etc.
	for(; i < (unsigned)(ch + 1); i++){
		if(ref & 1){
			value |= 1 << (ch - i);
		}
		ref >>= 1;
	}
	return value;
} 

static enum error_t init_crc_table (void)
{
	/*
	* polynomial used by CRC-32
	*/
	unsigned long int ulPolynomial = 0x04c11db7;
	unsigned long int i = 0L, j = 0L;

	// 256 values representing ASCII character codes.
	for(; i <= 0xFF; i++){
		crc32_table[i] = reflect(i, 8) << 24;
		for (j = 0; j < 8; j++){
			crc32_table[i] = (crc32_table[i] << 1) ^ (crc32_table[i] & (1 << 31) ? ulPolynomial : 0);
		}
		crc32_table[i] = reflect(crc32_table[i], 32);
	}
	return (BFS_SUCCESS);
} 

unsigned long int get_record_crc (const char* buffer, unsigned long int len)
{	
	unsigned long int  tmp_crc = 0xffffffff;
	while(len--)
		tmp_crc = (tmp_crc >> 8) ^ crc32_table[(tmp_crc & 0xFF) ^ *buffer++];

	return tmp_crc ^ 0xffffffff;
} 
/*
*	End CRC area
****************************************************************/

static enum error_t set_input_file(const char *fName)
{
	if (strlen(fName) <= 0){
		printf ("No input file name provided\n"); 
		return (BFS_ERROR);
	}else if (strlen(fName) > _MAX_PATH){
		printf ("Invalid input file name (to long?)\n"); 
		return (BFS_ERROR);
	}else 
		strcpy ((char*)inFileName, fName);
		return (BFS_SUCCESS);
}

static enum error_t set_output_file(const char *fName)
{
	if (strlen(fName) <= 0){
		printf ("No output file name provided\n"); 
		return (BFS_ERROR);
	}else if (strlen(fName) > _MAX_PATH){
		printf ("Invalid output file name (to long?)\n"); 
		return (BFS_ERROR);
	}else 
		strcpy ((char*)outFileName, fName);
		return (BFS_SUCCESS);
}

static enum error_t set_format(const char *f)
{
	if (strlen(f) > 1 
		|| (*f != '1' && *f != '2' && *f != '3' && *f != '4')){
		printf ("Invalid format specified\n"); 
		return (BFS_ERROR);
	}else 
		format = *f;
		if (format == '3') init_crc_table();
		return (BFS_SUCCESS);
}

static enum error_t set_silent_mode (const char *f)
{
	if (strlen(strupr((char*)f)) > 1 
	&& ((strcmp(f,"TRUE")) != 0 && (strcmp(f,"FALSE")) != 0)){
		printf ("Invalid silent mode specified\n"); 
		return (BFS_ERROR);
	}else{
		if (strcmp(f,"TRUE") == 0)
			silent_mode = TRUE;
		else
			silent_mode = FALSE;
		return (BFS_SUCCESS);
	}
}

/*
* build the show usage message
*/
void show_usage_string(FILE *fp)
{
	const struct option	*o;
	unsigned short int	i;
 
	(void) fprintf(stderr, "Btrieve File Saver version %s (c) dbcoretech ltd.\n"
	    "usage: btrfsave [options] \n", version());
  
	for (i = 0; i < NUM_OPTIONS; i++) {
		o = &known_options[i];
		(void) fprintf(fp, "  -%s\t%s", o->name, o->description);
		if (o->default_value != NULL)
			fprintf(fp, " (default: \"%s\")", o->default_value);
		fputc('\n', fp);
	}
	(void) fprintf(stderr, "\nexample: btrfsave -btrin myfile.btr -btrout dump.hex -format 3\n");
}

/*
 * Show usage string and exit.
 */
static void show_usage_and_exit(void)
{
	show_usage_string(stderr);
	exit(EXIT_FAILURE);
}

static const struct option * find_opt(const char *opt_name)
{
	int	i;
	for (i = 0; known_options[i].name != NULL; i++)
		if (!strcmp(opt_name, known_options[i].name))
			return (known_options + i);
 
	return (NULL);
}

enum error_t set_option(const char *opt, const char *val)
{
	const struct option	*option;
	int			i, error;
 
	DEBUG_TRACE((DEBUG_MGS_PREFIX "%s: [%s]->[%s]", __func__, opt, val));
	if (opt != NULL && (option = find_opt(opt)) != NULL) {
		i = (int) (option - known_options);
		error = option->setter ? option->setter(val) : BFS_SUCCESS;
  
		if (error != BFS_SUCCESS)
			printf("%s(%s): failure", __func__, opt);
	} else {
		printf("%s: No such option: [%s]", __func__, opt);
		error = BFS_NOT_FOUND;
	}
 
	return (error);
}

/*
 * process all arguments 
 */
static void process_command_line_arguments(char *argv[])
{
	unsigned long int	i; 
	const struct option	*o;

	/* First set default values */
	for (i = 0; i < NUM_OPTIONS; i++) {
		o = &known_options[i];
		if (o->default_value && o->setter)
			o->setter(o->default_value);
	}

	/* Now pass through the command line options */
	for (i = 1; argv[i] != NULL && argv[i][0] == '-'; i += 2)
		if (set_option(&argv[i][1], argv[i + 1]) != 1)
			exit(EXIT_FAILURE);
}

static void dump_record(char *buffer, unsigned long int len)
{

	if (!outFile || !fBuffer) return;
	switch (format){
		case '1':{ // BUTIL - save format
			sprintf(fBuffer, "%u,", len );      
			fwrite (fBuffer, strlen (fBuffer), 1, outFile);
			fwrite (buffer,1, len,outFile);
			strcpy (fBuffer, "\r\n");
			fwrite (fBuffer, strlen (fBuffer), 1, outFile);
		}break;
		case '2':{ // BUTIL - save format without len at the beginning
			fwrite (buffer,1, len, outFile);
			strcpy (fBuffer, "\r\n");
			fwrite (fBuffer, strlen (fBuffer), 1, outFile);			
		}break;
		case '3':{ // Hex Dump
			unsigned long int	cnt = 0;
			unsigned int		i = get_record_crc (buffer, len);
			sprintf (fBuffer, "%02x:", i);
			fwrite (fBuffer, strlen (fBuffer), 1, outFile);			
			for (; cnt < len; cnt++){
				((char*)&i)[0] = buffer[cnt];
				sprintf (fBuffer, "%02x", i);
				fwrite (fBuffer, strlen (fBuffer), 1, outFile);			
			}
			strcpy (fBuffer, "\r\n");
			fwrite (fBuffer, strlen (fBuffer), 1, outFile);						
		}break;
		case '4':{ // Hex Dump
			unsigned long int	cnt = 0;
			unsigned int		byte = 0L;
			for (; cnt < len; cnt++){
				((char*)&byte)[0] = buffer[cnt];
				sprintf (fBuffer, "%02x", byte);
				fwrite (fBuffer, strlen (fBuffer), 1, outFile);			
			}
			strcpy (fBuffer, "\r\n");
			fwrite (fBuffer, strlen (fBuffer), 1, outFile);						
		}break;
	}
}

unsigned long int   main(unsigned long int argc, char *argv[])
{
	char				*dataBuffer				= NULL;
	unsigned long int	dbLen					= MAX_REC_BUFFER_SIZE;
	unsigned long int	retval					= NO_ERROR;
	unsigned long int	recCnt					= 0L;
	CLIENT_STRUCT		clientID				= {0};
 
	if (argc <=4  || (!strcmp(argv[1], "-h")) || (!strcmp(argv[1], "--help")))
		show_usage_and_exit();

#ifndef _WIN32
	(void) signal(SIGCHLD, signal_handler);
#else  /* _WIN32 */
	(void) signal (SIGBREAK, signal_handler);
	(void) signal (SIGABRT, signal_handler);

#endif
	(void) signal(SIGTERM, signal_handler);
	(void) signal(SIGINT, signal_handler);

	process_command_line_arguments(argv);

	/* Open up the requested file */
	if ((retval = BF_OPEN (&clientID, (char*)&inFileName)) != NO_ERROR){
		printf ("File could not be opened (ErrorCode: %u)\n", retval);
		goto Exit;
	}
	/* 
	*	if we have successfully opened a file, allocate a buffer to retrieve 
	*	the records of that file. 
	*/ 
	if ((dataBuffer = (char*) malloc (MAX_REC_BUFFER_SIZE)) == NULL
		|| (fBuffer = (char*) malloc (MAX_FILE_BUFFER_SIZE)) == NULL){
		printf ("Error allocating memory\n");
		goto Exit;
	}else
		memset (dataBuffer, 0x00, MAX_REC_BUFFER_SIZE);

	/* open up the output file if give */
	if (strlen(outFileName)){
		if ((outFile = fopen ((char*)outFileName, "wb")) == NULL){
			printf ("Unable to create output file (%s)\n", (char*)outFileName);
			goto Exit;
		}
	}
	/* now get all the records */
	while ((retval = BF_GET_REC (&clientID, dataBuffer, &dbLen)) == NO_ERROR){
		if (!silent_mode ) printf ("Rec: %u - Len: %u \n", ++recCnt, dbLen);
		dump_record(dataBuffer, dbLen);
		dbLen = MAX_REC_BUFFER_SIZE;
	}

Exit:
	
	/* close the file and release all resources */
	BF_CLOSE (&clientID);

	if (dataBuffer) free (dataBuffer);
	if (fBuffer) free (fBuffer);
	if (outFile) fclose (outFile);
	if (!silent_mode) {
		printf ("press any key to exit the app\n");
		getch();
	}
	return FALSE;
}