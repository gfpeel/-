#define MAX_LINES 50 // max input line
#define MAX_LINE_LENGTH 50  // max length of line

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int R_formats(char*);
int R_formats_sh(char*);
int I_formats(char*);
int I_formats_b(char*);
int I_formats_lw_sw(char*);
int I_formats_others(char*);
void decimal2binary(int);

int main(int argc, char* argv[]){

	if(argc != 2){
		printf("Usage: ./runfile <assembly file>\n"); //Example) ./runfile /sample_input/example1.s
		printf("Example) ./runfile ./sample_input/example1.s\n");
		exit(0);
	}
	else
	{

		// To help you handle the file IO, the deafult code is provided.
		// If we use freopen, we don't need to use fscanf, fprint,..etc. 
		// You can just use scanf or printf function 
		// ** You don't need to modify this part **
		// If you are not famailiar with freopen,  you can see the following reference
		// http://www.cplusplus.com/reference/cstdio/freopen/

		//For input file read (sample_input/example*.s)

		char *file=(char *)malloc(strlen(argv[1])+3);
		strncpy(file,argv[1],strlen(argv[1]));

		if(freopen(file, "r", stdin) == 0) {
			printf("File open Error!\n");
			exit(1);
		}

		//From now on, if you want to read string from input file, you can just use scanf function.
		/* input codes */
		char input[MAX_LINES][MAX_LINE_LENGTH];
    	int line = 0, charactor = 0, chars = 0;
		int c = 0;
		int input_maxline = 0;

		for (line = 0; line < MAX_LINES; line++) {
			if (fgets(input[line], sizeof(input[line]), stdin) == NULL)
				break;
			if (input[line][0] == '\n') {
				break;
			}
			input[line][strcspn(input[line], "\n")] = '\0';
    	}


		input_maxline = line-1; // # of line

		/* parsing .data */
		const char str_data[] = "data";
		const char str_text[] = ".text";
		const char str_word[] = ".word";
		const char str_array[] = "array";

		int data[10][10];
		int i_data = 0;
		int j_data = 0;

			/*	make a array such as:
				\0	\0	\0	\0	\0	\0	...
				0	1	43	\0	\0	\0	...
				0	1	34	\0	\0	\0	...
				0	2	21	42	\0	\0	...
				1	3	65	41	12	\0	...	*/
		line = 1; // exept first line: .data

		while (strstr(input[line], str_text) == NULL) {

			/* data: not array */
			if (strstr(input[line], str_data) != NULL) {
				i_data++;
				j_data = 2;
				data[i_data][0] = 0;
			}
			/* data: array */
			else if (strstr(input[line], str_array) != NULL) {
				i_data++;
				j_data = 2;
				data[i_data][0] = 1;
			}
			data[i_data][j_data] = (int) strtol(strstr(input[line], str_word) + 6, NULL, 0);
			data[i_data][1] = j_data - 1;
			j_data++;			
			line++;
		}
		data[i_data+1][0] = 2;

		//line: next line '.text'
		int text_line = line + 1;

		/* parsing .text */
			/* count # of line */

		int assembly_arr[MAX_LINES];

		const char str_addi[] = "addi";
		const char str_add[] = "add";
		const char str_and[] = "and";
		const char str_andi[] = "andi";
		const char str_beq[] = "beq";
		const char str_bne[] = "bne";
		const char str_j[] = "j";
		const char str_jal[] = "jal";
		const char str_jr[] = "jr";
		const char str_lui[] = "lui";
		const char str_lw[] = "lw";
		const char str_la[] = "la	$";
		const char str_nor[] = "nor";
		const char str_or[] = "or	$";
		const char str_ori[] = "ori";
		const char str_slti[] = "slti";
		const char str_slt[] = "slt";
		const char str_sll[] = "sll";
		const char str_srl[] = "srl";
		const char str_sw[] = "sw";
		const char str_sub[] = "sub";
		
		struct func {
			int num;
			char func_name[10];
		};
		struct func func_list[10];
		int func_num = 0;
		char* func_name_end;
		char* cpy_ptr;
		int data_num;
		int until_data_num;
		int la_list[10];
		int la_list_i = -1;
		int total_size = 1;
		line = text_line;
			/*	make a list such as:
				??????
				num: 10,	func_name: main
				num: 5,		func_name: lab1
				num: 3,		func_name: lab2	*/
		while (line < input_maxline) {
			if ((func_name_end = strchr(input[line], ':')) != NULL) {
				func_num++;
				cpy_ptr = input[line];
				while (cpy_ptr != func_name_end) {
            		func_list[func_num].func_name[cpy_ptr - input[line]] = *cpy_ptr;
            		cpy_ptr++;
       			}
        		func_list[func_num].func_name[cpy_ptr - input[line]] = '\0';
				func_list[func_num].num = 0;
			}
			else {
				total_size++;
				func_list[func_num].num++;
				/* operation "la" could transform lui + ori */
				if (strstr(input[line], str_la) != NULL) {
					la_list_i++;
					/* la data5 */
					if ((func_name_end = strstr(input[line], str_data)) != NULL) {
						data_num = (*(func_name_end + 4)) - '0';	//ex) data5: ~~ -> data_num == 5
						i_data = 1;
						until_data_num = 0;
						while (data_num > 1) {
							until_data_num += data[i_data][1];
							i_data++;
							/* consider just data (not array) */
							if (data[i_data][0] == 0) {
								data_num--;
							}
						}
						if ((until_data_num * 4) % 65536 != 0 ) {
							func_list[func_num].num++;	
							total_size++;
						}
						la_list[la_list_i] = (until_data_num * 4);
					}
					/* la array5 */
					else if (strstr(input[line], str_array) != NULL) {
						func_name_end = strstr(input[line], str_array);
						data_num = (*(func_name_end + 5)) - '0';	//ex) data5: ~~ -> data_num == 5
						i_data = 1;
						until_data_num = 0;
						while (data_num > 1) {
							until_data_num += data[i_data][1];
							i_data++;
							
							/* consider just data (not array) */
							if (data[i_data][0] == 1) {
								data_num--;
							}
						}
						if ((until_data_num * 4) % 65536 != 0 ) {
							func_list[func_num].num++;
							total_size++;
						}
						la_list[la_list_i] = (until_data_num * 4);
					}
					/* la (some address) */
					else {
						if ((((int) strtol((strstr(input[line], str_la) + 2), NULL, 0) - '0') * 4) % 65536 != 0) {
							func_list[func_num].num++;
							la_list[la_list_i] = (until_data_num * 4);
						}
					}
				}
			}
			line++;
		}
		line = text_line;

			/* object code */
		struct func current_func;
		int objectcode_formats = 0;
		int assembly_arr_i = 0;
		la_list_i = 0;
		int la_r;
		int la_i;
		char copy_func_name[10];
		int find_name_i = 0;
		int jump_addr;
		int current_addr;

		while (line < input_maxline + 1) {
			objectcode_formats = 0;
			if ((func_name_end = strchr(input[line], ':')) != NULL) {
				cpy_ptr = input[line];
				while (cpy_ptr != func_name_end) {
					current_func.func_name[cpy_ptr - input[line]] = *cpy_ptr;
					cpy_ptr++;
				}
				current_func.func_name[cpy_ptr - input[line]] = '\0';
				current_func.num = 0;
			}
			current_func.num++;

			if (input[line][0] != '	') {
				line++;

			}
			else if (strstr(input[line], str_addi) != NULL){
				objectcode_formats = I_formats(input[line]);
				objectcode_formats |= (0x8 << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_add) != NULL){
				
				objectcode_formats = R_formats(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x20;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_andi) != NULL){
				objectcode_formats = I_formats(input[line]);
				objectcode_formats |= (0xc << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_and) != NULL){
				objectcode_formats = R_formats(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x24;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_beq) != NULL){
				objectcode_formats = I_formats_b(input[line]);
				strcpy(copy_func_name, strchr(strchr(input[line], ',') + 1, ',') + 2);
				jump_addr = 0;
				for (find_name_i = 1; strstr(func_list[find_name_i].func_name, copy_func_name) == NULL; find_name_i++) {
					jump_addr += func_list[find_name_i].num;
				}
				memset(copy_func_name, '\0', 9);
				current_addr = 0;
				for (find_name_i = 1; strstr(func_list[find_name_i].func_name, current_func.func_name) == NULL; find_name_i++) {
					current_addr += func_list[find_name_i].num;
				}
				objectcode_formats |= (0x0000ffff & (((jump_addr + 1) - (current_addr + current_func.num))));
				objectcode_formats |= (0x4 << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_bne) != NULL){
				objectcode_formats = I_formats_b(input[line]);
				strcpy(copy_func_name, strchr(strchr(input[line], ',') + 1, ',') + 2);
				jump_addr = 0;
				for (find_name_i = 1; strstr(func_list[find_name_i].func_name, copy_func_name) == NULL; find_name_i++) {
					jump_addr += func_list[find_name_i].num;
				}
				memset(copy_func_name, '\0', 9);
				current_addr = 0;
				for (find_name_i = 1; strstr(func_list[find_name_i].func_name, current_func.func_name) == NULL; find_name_i++) {
					current_addr += func_list[find_name_i].num;
				}
				objectcode_formats |= ( 0x0000ffff & (((jump_addr + 1) - (current_addr + current_func.num))));
				objectcode_formats |= (0x5 << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_jal) != NULL){
				strcpy(copy_func_name, strstr(input[line], str_jal) + 4);
				jump_addr = 0;
				for (find_name_i = 1; strstr(func_list[find_name_i].func_name, copy_func_name) == NULL; find_name_i++) {
					jump_addr += func_list[find_name_i].num;
				}
				memset(copy_func_name, '\0', 9);
				objectcode_formats |= (jump_addr);
				objectcode_formats |= (0x100000);
				objectcode_formats |= (0x3 << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_jr) != NULL){
				objectcode_formats = ((int) strtol(strchr(input[line], '$') + 1, NULL, 0) << 21);
				objectcode_formats |= (0x8);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_j) != NULL){
				strcpy(copy_func_name, strstr(input[line], str_j) + 4);
				jump_addr = 0;
				for (find_name_i = 1; strstr(func_list[find_name_i].func_name, copy_func_name) == NULL; find_name_i++) {
					jump_addr += func_list[find_name_i].num;
				}
				memset(copy_func_name, '\0', 10);
				objectcode_formats |= (jump_addr);
				objectcode_formats |= (1 << 20);
				objectcode_formats |= (0x02 << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_lui) != NULL){
				objectcode_formats = I_formats_others(input[line]);
				objectcode_formats |= (0xf << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_lw) != NULL){
				objectcode_formats = I_formats_lw_sw(input[line]);
				objectcode_formats |= (0x23 << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_nor) != NULL){
				objectcode_formats = R_formats(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x27;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_ori) != NULL){
				objectcode_formats = I_formats(input[line]);
				objectcode_formats |= (0xd << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_slti) != NULL){
				objectcode_formats = I_formats(input[line]);
				objectcode_formats |= (0xa << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_slt) != NULL){
				objectcode_formats = R_formats(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x2a;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_sll) != NULL){
				objectcode_formats = R_formats_sh(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x00;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_srl) != NULL){
				objectcode_formats = R_formats_sh(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x02;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_sw) != NULL){
				objectcode_formats = I_formats_lw_sw(input[line]);
				objectcode_formats |= (0x2b << 26);
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_sub) != NULL) {
				objectcode_formats = R_formats(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x22;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_la) != NULL){
				/* lui + ori */
				if (la_list[la_list_i] % 65536 != 0 ) {
					objectcode_formats = (strtol(strchr(input[line], '$') + 1, NULL, 0)) << 16;
					objectcode_formats |= (1 << 12);
					objectcode_formats |= (0xf << 26);	
					assembly_arr[assembly_arr_i] = objectcode_formats;
					assembly_arr_i++;
					objectcode_formats = (strtol(strchr(input[line], '$') + 1, NULL, 0)) << 16;
					objectcode_formats |= (objectcode_formats << 5);
					objectcode_formats |= (la_list[la_list_i] % 65536);
					objectcode_formats |= (0xd << 26);
				}
				/* lui */
				else {
					objectcode_formats = (strtol(strchr(input[line], '$') + 1, NULL, 0)) << 16;
					objectcode_formats |= (1 << 12);
					objectcode_formats |= (0xf << 26);					
				}
				la_list_i++;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}
			else if (strstr(input[line], str_or) != NULL){
				objectcode_formats = R_formats(input[line]);
				objectcode_formats |= (0x0 << 26);
				objectcode_formats |= 0x25;
				assembly_arr[assembly_arr_i] = objectcode_formats;
				assembly_arr_i++;
				line++;
			}

			
		}

		// For output file write 
		// You can see your code's output in the sample_input/example#.o 
		// So you can check what is the difference between your output and the answer directly if you see that file
		// make test command will compare your output with the answer
		file[strlen(file)-1] ='o';
		freopen(file,"w",stdout);

		int text_size = 0;
		int data_size = 0;
		int data_size_i = 0;
		int print_i;
		int print_j;
		decimal2binary(total_size * 4);

		for (data_size_i = 1; data_size_i < (text_line - 1); data_size_i++) {
			data_size += data[data_size_i][1];
		}
		decimal2binary(data_size * 4);
		for (print_i = 0; print_i < assembly_arr_i; print_i++) {
			decimal2binary(assembly_arr[print_i]);
		}
		for (print_i = 1; data[print_i][0] != 2; print_i++) {
			for (print_j = 2; (print_j - 2) < data[print_i][1]; print_j++) {
				decimal2binary(data[print_i][print_j]);
			}
		}

	}
	return 0;
}
// R-formats
int R_formats(char* assembly_arr) {
	int rd, rs, rt;
	char* ptr1 = strchr(assembly_arr, '$') + 1;
	rd = (int) strtol(ptr1, NULL, 0);
	char* ptr2 = strchr(ptr1, '$') + 1;
	rs = (int) strtol(ptr2, NULL, 0);
	char* ptr3 = strchr(ptr2, '$') + 1;
	rt = (int) strtol(ptr3, NULL, 0);
	return (((rs << 21) + (rt << 16) ) + (rd << 11));
}

int R_formats_sh(char* assembly_arr) {
	int rd, rt, sh;
	char* ptr1 = strchr(assembly_arr, '$') + 1;
	rd = (int) strtol(ptr1, NULL, 0);
	char* ptr2 = strchr(ptr1, '$') + 1;
	rt = (int) strtol(ptr2, NULL, 0);
	char* ptr3 = strchr(ptr2, ',') + 1;
	sh = (int) strtol(ptr3, NULL, 0);

	return (((rt << 16) | (rd << 11)) | (sh << 6));
}

int I_formats(char* assembly_arr) {
	int rs, rt, imm;
	char* ptr1 = strchr(assembly_arr, '$') + 1;
	rt = (int) strtol(ptr1, NULL, 0);
	char* ptr2 = strchr(ptr1, '$') + 1;
	rs = (int) strtol(ptr2, NULL, 0);
	char* ptr3 = strchr(ptr2, ',') + 1;
	imm = (int) strtol(ptr3, NULL, 0);
	
	return (((rs << 21) | (rt << 16) )| imm);
}

int I_formats_lw_sw(char* assembly_arr) {
	int rs, rt, imm;
	char* ptr1 = strchr(assembly_arr, '$') + 1;
	rt = (int) strtol(ptr1, NULL, 0);
	char* ptr2 = strchr(ptr1, ',') + 1;
	imm = (int) strtol(ptr2, NULL, 0);
	char* ptr3 = strchr(ptr2, '$') + 1;
	rs = (int) strtol(ptr3, NULL, 0);
	
	return (((rs << 21) | (rt << 16)) | (0xffff & imm));
}

int I_formats_others(char* assembly_arr) {
	int rt, imm;
	char* ptr1 = strchr(assembly_arr, '$') + 1;
	rt = (int) strtol(ptr1, NULL, 0);
	char* ptr2 = strchr(assembly_arr, ',') + 1;
	if (strtol(ptr2, NULL, 0) == 0) {
		imm = 0;
	}
	else {
		imm = (int) strtol(ptr2, NULL, 0);
	}
	
	return ((rt << 16) | imm);
}

int I_formats_b(char* assembly_arr) {
	int rs, rt;
	char* ptr1 = strchr(assembly_arr, '$') + 1;
	rs = (int) strtol(ptr1, NULL, 0);
	char* ptr2 = strchr(ptr1, '$') + 1;
	rt = (int) strtol(ptr2, NULL, 0);
	
	return ((rs << 21) | (rt << 16));
}

void decimal2binary(int decimal) {
	int i;
	for (i = 0; i < 32; i++) {
		printf("%d", (decimal >> (31 - i)) & 1);
	}
}
