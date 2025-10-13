
module test_while(
  input logic clk,
  input logic [7:0] n,
  output logic [7:0] sum
);
  always @(posedge clk) begin
    sum = 8'd0;
    while (sum < n)
      sum = sum + 8'd1;
  end
endmodule
