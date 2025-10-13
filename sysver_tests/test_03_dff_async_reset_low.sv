
module test_dff_rstn(
  input logic clk,
  input logic rstn,
  input logic d,
  output logic q
);
  always_ff @(posedge clk or negedge rstn) begin
    if (!rstn)
      q <= 1'b0;
    else
      q <= d;
  end
endmodule
