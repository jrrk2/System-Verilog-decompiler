
module test_dff_sync_rst(
  input logic clk,
  input logic rst,
  input logic d,
  output logic q
);
  always @(posedge clk) begin
    if (rst)
      q <= 1'b0;
    else
      q <= d;
  end
endmodule
