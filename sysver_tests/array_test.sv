module array_test 
    #(
        parameter int WIDTH = 8,
        parameter int SIZE = 4
    ) (
    input  logic        clk, write,
    input  logic [1:0]  addr,
    input  logic [7:0]  data,
    output logic [7:0]  result
);

    // Test Issue 1: Array declaration with size
    logic [WIDTH-1:0] memory [0:SIZE-1];
    
    always @(posedge clk)
      if (write)
	memory[addr] <= data;
      else
	result <= memory[addr];
   
endmodule
