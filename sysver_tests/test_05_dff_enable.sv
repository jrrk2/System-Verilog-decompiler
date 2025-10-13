
module test_dff_en(
  input logic clk,
  input logic en,
  input logic d,
  output logic q
);
  always_ff @(posedge clk) begin
    if (en)
      q <= d;
  end
endmodule
