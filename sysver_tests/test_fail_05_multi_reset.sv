
module test_multirst(
  input logic clk,
  input logic rst1,
  input logic rst2,
  input logic d,
  output logic q
);
  always @(posedge clk or posedge rst1 or posedge rst2) begin
    if (rst1 || rst2)
      q <= 1'b0;
    else
      q <= d;
  end
endmodule
