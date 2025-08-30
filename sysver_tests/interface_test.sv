interface simple_if(input logic clk);
    logic [7:0] data, cnt;
    modport master (input clk, output data);
    modport slave (input clk, input data, output cnt);
endinterface

module producer(simple_if.master sif);
    always_ff @(posedge sif.clk) sif.data <= sif.data + 1;
endmodule

module consumer(simple_if.slave sif);
    always_ff @(posedge sif.clk) sif.cnt <= sif.data;
endmodule

module top(input clk, output [7:0] cnt);
    simple_if sif(clk);
    producer p(sif);
    consumer c(sif);
    assign cnt = sif.cnt;
   
endmodule
