/***************************************************************/
/*                                                             */
/*   MIPS-32 Instruction Level Simulator                       */
/*                                                             */
/*   CS311 KAIST                                               */
/*   parse.c                                                   */
/*                                                             */
/***************************************************************/

#include <stdio.h>

#include "util.h"
#include "parse.h"

int text_size;
int data_size;

instruction parsing_instr(const char *buffer, const int index)
{
    instruction instr;
    /* Implement your function here */
	/* chararr2uint */
	instr.value = (uint32_t) fromBinary(buffer);
	/* 16bit, opcode */
	instr.opcode = (instr.value >> 26) & (0x3f);
	/* 16bit, func_code, R-type */
	if (instr.opcode == 0)
	instr.func_code = instr.value & (0x3f);
	/* union r_t */
	  /* 32bit, struct r_i, R-type, I-type */
	if (instr.opcode != 0x2 && instr.opcode != 0x3) {
		/* 8bit, rs: first register */
		instr.r_t.r_i.rs = (instr.value >> 21) & (0x1f);
		/* 8bit, rt: second register */
		instr.r_t.r_i.rt = (instr.value >> 16) & (0x1f);
		/* union r_i */
		  /* 16bit, imm */
		if (instr.opcode != 0)
			instr.r_t.r_i.r_i.imm = instr.value & (0xffff);
		  /* 16bit, struct r */
		else {
			/* 8bit, rd: register for destination operand */
			instr.r_t.r_i.r_i.r.rd = (instr.value >> 11) & (0x1f);
			/* 8bit, shamt: shift value */
			instr.r_t.r_i.r_i.r.shamt = (instr.value >> 6) & (0x1f);
		}
	}
	  /* 32bit, J-type */
	else {
		instr.r_t.target = (instr.value << 6) >> 6;
	}

	mem_write_32(MEM_TEXT_START + index, instr.value);
	 
    return instr;
}

void parsing_data(const char *buffer, const int index)
{
	/* Implement your function here */
    mem_write_32(MEM_DATA_START + index, fromBinary(buffer));
}

void print_parse_result()
{
    int i;
    printf("Instruction Information\n");

    for(i = 0; i < text_size/4; i++)
    {
	printf("INST_INFO[%d].value : %x\n",i, INST_INFO[i].value);
	printf("INST_INFO[%d].opcode : %d\n",i, INST_INFO[i].opcode);

	switch(INST_INFO[i].opcode)
	{
	    //Type I
	    case 0x8:		// ADDI
	    case 0xc:		// ANDI
	    case 0xf:		// LUI	
	    case 0xd:		// ORI
	    case 0xa:		// SLTI
	    case 0x23:		// LW	
	    case 0x2b:		// SW
	    case 0x4:		// BEQ
	    case 0x5:		// BNE
		printf("INST_INFO[%d].rs : %d\n",i, INST_INFO[i].r_t.r_i.rs);
		printf("INST_INFO[%d].rt : %d\n",i, INST_INFO[i].r_t.r_i.rt);
		printf("INST_INFO[%d].imm : %d\n",i, INST_INFO[i].r_t.r_i.r_i.imm);
		break;

    	    //TYPE R
	    case 0x0:		// ADD, AND, NOR, OR, SLT, SLL, SRL, SUB
		printf("INST_INFO[%d].func_code : %d\n",i, INST_INFO[i].func_code);
		printf("INST_INFO[%d].rs : %d\n",i, INST_INFO[i].r_t.r_i.rs);
		printf("INST_INFO[%d].rt : %d\n",i, INST_INFO[i].r_t.r_i.rt);
		printf("INST_INFO[%d].rd : %d\n",i, INST_INFO[i].r_t.r_i.r_i.r.rd);
		printf("INST_INFO[%d].shamt : %d\n",i, INST_INFO[i].r_t.r_i.r_i.r.shamt);
		break;

    	    //TYPE J
	    case 0x2:		// J
	    case 0x3:		// JAL
		printf("INST_INFO[%d].target : %d\n",i, INST_INFO[i].r_t.target);
		break;

	    default:
		printf("Not available instruction\n");
		assert(0);
	}
    }

    printf("Memory Dump - Text Segment\n");
    for(i = 0; i < text_size; i+=4)
	printf("text_seg[%d] : %x\n", i, mem_read_32(MEM_TEXT_START + i));
    for(i = 0; i < data_size; i+=4)
	printf("data_seg[%d] : %x\n", i, mem_read_32(MEM_DATA_START + i));
    printf("Current PC: %x\n", CURRENT_STATE.PC);
}
