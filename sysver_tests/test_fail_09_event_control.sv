
module test_event(
  input logic clk,
  input logic event_trigger,
  output logic q
);
  always begin
    @(posedge event_trigger);
    q <= 1'b1;
  end
endmodule
