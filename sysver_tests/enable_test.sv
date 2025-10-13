module enable_test 
    #(
        parameter int WIDTH = 8
    ) (
    input  logic        clk, enable,
    input  logic [WIDTH-1:0]  data,
    output logic [WIDTH-1:0]  result
);

    always @(posedge clk)
      if (enable)
	result <= data;
   
endmodule
