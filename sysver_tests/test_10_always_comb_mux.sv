
module test_comb_mux(
  input logic sel,
  input logic [7:0] a,
  input logic [7:0] b,
  output logic [7:0] y
);
  always_comb begin
    if (sel)
      y = a;
    else
      y = b;
  end
endmodule
