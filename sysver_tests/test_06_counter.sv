
module test_counter(
  input logic clk,
  input logic rst,
  output logic [7:0] count
);
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      count <= 8'h0;
    else
      count <= count + 8'h1;
  end
endmodule
