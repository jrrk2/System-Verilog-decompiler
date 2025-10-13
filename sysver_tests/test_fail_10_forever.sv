
module test_forever(
  input logic clk,
  output logic q
);
  always begin
    forever @(posedge clk)
      q <= ~q;
  end
endmodule
