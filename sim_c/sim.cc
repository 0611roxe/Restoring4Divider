#include <iostream>
#include <bitset>
#include <climits>
#include <random>

#include <verilated.h>
#include "VRestoringDivider.h"
#ifdef VCD
	#include "verilated_vcd_c.h"
	VerilatedVcdC* tfp = nullptr;
#endif

#include "sim.h"

using namespace std; 

// init pointers
const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
const std::unique_ptr<VRestoringDivider> top{new VRestoringDivider{contextp.get(), "RestoringDivider"}};

/* sim initial */
void sim_init(int argc, char *argv[]) {
	top->reset = 1;
	top->clock = 0;
#ifdef VCD
	Verilated::mkdir("logs");
	contextp->traceEverOn(true);
	tfp = new VerilatedVcdC;
	top->trace(tfp, 99);
    tfp->open("logs/top.vcd");
#endif
	Verilated::commandArgs(argc,argv);
}

/* sim exit */
void sim_exit() {
	// finish work, delete pointer
	top->final();
#if VCD
	tfp->close();
	tfp = nullptr;
#endif
}

void single_cycle() {
	contextp->timeInc(1);
	top->clock = 0; top->eval();
#ifdef VCD
 tfp->dump(contextp->time());
#endif

	contextp->timeInc(1);
	top->clock = 1; top->eval();
#ifdef VCD
 tfp->dump(contextp->time());
#endif
}

void reset(int n) {
	top->reset = 1;
	while (n-- > 0) single_cycle();
	top->reset = 0;
	top->eval();
}

void sim_main(int argc, char *argv[]) {
	sim_init(argc, argv);
	reset(10);

	int sim_time = 0;

	top->io_in_valid = 1;
    top->io_in_bits_0 = -163;
	top->io_in_bits_1 = 6;
	top->io_sign = 1;
	
	#define upper_32_bits(n) ((uint32_t)(((n) >> 16) >> 16))
	#define lower_32_bits(n) ((uint32_t)((n) & 0xffffffff))
	/* main loop */
	while (top->io_in_valid) {
		single_cycle();
		sim_time++;
		if(top->io_out_valid){
			printf("simtime:%d\t%d/%d = %d/%d\n", sim_time, top->io_in_bits_0, top->io_in_bits_1, lower_32_bits(top->io_out_bits), upper_32_bits(top->io_out_bits));
			break;
		}
	}
	sim_exit();
}