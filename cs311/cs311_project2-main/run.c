/***************************************************************/
/*                                                             */
/*   MIPS-32 Instruction Level Simulator                       */
/*                                                             */
/*   CS311 KAIST                                               */
/*   run.c                                                     */
/*                                                             */
/***************************************************************/

#include <stdio.h>

#include "util.h"
#include "run.h"

/***************************************************************/
/*                                                             */
/* Procedure: get_inst_info                                    */
/*                                                             */
/* Purpose: Read instruction information                       */
/*                                                             */
/***************************************************************/
instruction* get_inst_info(uint32_t pc) 
{ 
    return &INST_INFO[(pc - MEM_TEXT_START) >> 2];
}

/***************************************************************/
/*                                                             */
/* Procedure: process_instruction                              */
/*                                                             */
/* Purpose: Process one instruction                             */
/*                                                             */
/***************************************************************/
int SignExtImm(short imm)
{
    return ((int) imm) << 16 >> 16;
}
int ZeroExtImm(short imm)
{
    return ((int) imm) & 0xffff;
}
int BranchAddr(short imm)
{
    return ((int) imm) << 16 >> 14;
}
int JumpAddr(uint32_t PC, uint32_t addr)
{
    return ((((int) PC) + 4) >> 28 << 28) | (((int) addr) << 2);
}

void process_instruction(){
	/* Implement your function here */
    instruction _Instr = INST_INFO[(CURRENT_STATE.PC - MEM_TEXT_START) / 4];
    if (mem_read_32(CURRENT_STATE.PC + 4) == 0)
        RUN_BIT = FALSE;
    //printf("num: %d\n", (CURRENT_STATE.PC - MEM_TEXT_START) / 4);
    short _opcode = _Instr.opcode;
    //printf("opcode: %#x\n", (int) _opcode);
    unsigned char _rs;
    unsigned char _rt;
    unsigned char _rd;
    unsigned char _shamt;
    short _funct;
    short _imm;
    uint32_t _addr;
    char PC_incr = 1;
    switch(_opcode) {
        // Type R
        case 0:
            _rs = _Instr.r_t.r_i.rs;
            _rt = _Instr.r_t.r_i.rt;
            _rd = _Instr.r_t.r_i.r_i.r.rd;
            _shamt = _Instr.r_t.r_i.r_i.r.shamt;
            _funct = _Instr.func_code;
            break;
        // Type J
	    case 0x2:
	    case 0x3:
            _addr = _Instr.r_t.target;
            break;
        // Type I
        default:
            _rs = _Instr.r_t.r_i.rs;
            _rt = _Instr.r_t.r_i.rt;
            _imm = _Instr.r_t.r_i.r_i.imm;
    }
    //printf("opcode: %x\n", _opcode);
    /* Type R */
    if (_opcode == 0 ) {
        //printf("funct: %d\n", _funct);
    switch (_funct) {
        case 0x20:      //ADD
            CURRENT_STATE.REGS[_rd] = CURRENT_STATE.REGS[_rs] + CURRENT_STATE.REGS[_rt];
            break;
        case 0x24:      //AND
            CURRENT_STATE.REGS[_rd] = CURRENT_STATE.REGS[_rs] & CURRENT_STATE.REGS[_rt];
            break;
        case 0x08:      //JR
            CURRENT_STATE.PC = CURRENT_STATE.REGS[_rs];
            PC_incr = 0;
            break;
        case 0x27:      //NOR
            CURRENT_STATE.REGS[_rd] = ~(CURRENT_STATE.REGS[_rs] | CURRENT_STATE.REGS[_rt]);
            break;
        case 0x25:      //OR
            CURRENT_STATE.REGS[_rd] = CURRENT_STATE.REGS[_rs] | CURRENT_STATE.REGS[_rt];
            break;
        case 0x2a:      //SLT
            CURRENT_STATE.REGS[_rd] = ((int) CURRENT_STATE.REGS[_rs] < (int) CURRENT_STATE.REGS[_rt] ? 1 : 0);
            break;
        case 0x00:      //SLL
            CURRENT_STATE.REGS[_rd] = CURRENT_STATE.REGS[_rt] << _shamt;
            break;
        case 0x02:      //SRL
            CURRENT_STATE.REGS[_rd] = CURRENT_STATE.REGS[_rt] >> _shamt;
            break;
        case 0x22:      //SUB
            CURRENT_STATE.REGS[_rd] = CURRENT_STATE.REGS[_rs] - CURRENT_STATE.REGS[_rt];
            break;
    }
    }
    /* Type I, J */
    else{
    switch(_opcode)
	{
	    //Type I
	    case 0x8:		// ADDI
            CURRENT_STATE.REGS[_rt] = CURRENT_STATE.REGS[_rs] + SignExtImm(_imm);
            break;
	    case 0xc:		// ANDI
            CURRENT_STATE.REGS[_rt] = CURRENT_STATE.REGS[_rs] & ZeroExtImm(_imm);
            break;
	    case 0xf:		// LUI	
            CURRENT_STATE.REGS[_rt] = _imm << 16;
            break;
	    case 0xd:		// ORI
            CURRENT_STATE.REGS[_rt] = CURRENT_STATE.REGS[_rs] | ZeroExtImm(_imm);
            break;
	    case 0xa:		// SLTI
            if ((int) CURRENT_STATE.REGS[_rs] < SignExtImm(_imm)) {
                CURRENT_STATE.REGS[_rt] = 1;
            }
            else {
                CURRENT_STATE.REGS[_rt] = 0;
            }
            break;
	    case 0x23:		// LW	
            CURRENT_STATE.REGS[_rt] = mem_read_32(CURRENT_STATE.REGS[_rs] + SignExtImm(_imm));
            break;
	    case 0x2b:		// SW
            mem_write_32(CURRENT_STATE.REGS[_rs] + SignExtImm(_imm), CURRENT_STATE.REGS[_rt]);
            break;
	    case 0x4:		// BEQ
            BRANCH_INST(CURRENT_STATE.REGS[_rs] == CURRENT_STATE.REGS[_rt], BranchAddr(_imm) + CURRENT_STATE.PC, 0);
            break;
	    case 0x5:		// BNE
            BRANCH_INST(CURRENT_STATE.REGS[_rs] != CURRENT_STATE.REGS[_rt], BranchAddr(_imm) + CURRENT_STATE.PC, 0);
    		break;

    	//Type J
	    case 0x2:		// J
            CURRENT_STATE.PC = JumpAddr(CURRENT_STATE.PC, _addr);
            PC_incr = 0;
            break;
	    case 0x3:		// JAL
            CURRENT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
            CURRENT_STATE.PC = JumpAddr(CURRENT_STATE.PC, _addr);
            PC_incr = 0;
		    break;

	    default:
        printf("%x", (int) _opcode);
		printf("Not available instruction\n");
		assert(0);
	}
    }
    if (PC_incr)
        CURRENT_STATE.PC += 4;
}
