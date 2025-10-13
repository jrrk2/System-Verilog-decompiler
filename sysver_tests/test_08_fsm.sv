
module test_fsm(
  input logic clk,
  input logic rst,
  input logic in,
  output logic [1:0] state
);
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      state <= 2'b00;
    else begin
      case (state)
        2'b00: state <= in ? 2'b01 : 2'b00;
        2'b01: state <= in ? 2'b10 : 2'b00;
        2'b10: state <= in ? 2'b11 : 2'b00;
        2'b11: state <= 2'b00;
      endcase
    end
  end
endmodule
