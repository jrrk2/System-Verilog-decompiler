
module test_display(
  input logic clk,
  input logic [7:0] data
);
  always @(posedge clk)
    $display("Data = %h", data);
endmodule
