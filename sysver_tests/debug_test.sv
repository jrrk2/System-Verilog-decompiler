// debug_test.sv - Focused test cases for decompiler issues
// Compile with: verilator --dump-tree-json -Wall --lint-only debug_test.sv

package debug_pkg;
    // Test 1: Simple typedef
    typedef logic [7:0] byte_t;
    
    // Test 2: Enum with specific values
    typedef enum logic [1:0] {
        STATE_A = 2'b00,
        STATE_B = 2'b01
    } simple_state_t;
    
    // Test 3: Packed struct
    typedef struct packed {
        logic       valid;
        logic [3:0] data;
    } simple_packet_t;
endpackage

module debug_test 
    import debug_pkg::*;
    #(
        parameter int WIDTH = 8,
        parameter int SIZE = 4
    ) (
    input  logic        clk,
    input  logic        rst_n,
    input  logic        enable,
    output logic [7:0]  result,
    output logic [7:0]  comb_result,
    output simple_state_t state_out
);

    // Test Issue 1: Array declaration with size
    logic [7:0] memory [0:15];  // Should become [0:15] not [0:255]
    
    // Test Issue 2: Small array
    logic [3:0] small_mem [0:3];
    
    // Test Issue 3: Internal loop variable
    initial begin
        // This creates a BLOCKTEMP variable __Vrepeat0
        repeat(3) @(posedge clk);
    end
    
    // Test Issue 4: Simple state machine
    simple_state_t state_q, state_d;
    logic [7:0] counter;
    
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state_q <= STATE_A;
            counter <= 8'h0;
        end else begin
            state_q <= state_d;
            if (enable) begin
                counter <= counter + 8'h1;
            end
        end
    end
    
    // Test Issue 5: Combinational logic with case
    always_comb begin
        state_d = state_q;
        result = 8'h0;
        
        case (state_q)
            STATE_A: begin
                if (enable) begin
                    state_d = STATE_B;
                    result = counter;
                end
            end
            STATE_B: begin
                state_d = STATE_A;
                result = ~counter;
            end
            default: begin
                state_d = STATE_A;
            end
        endcase
    end
    
    // Test Issue 6: Memory write with byte enables
    logic [1:0] be;
    logic       we;
    logic [3:0] addr;
    logic [7:0] wdata;
    
    always_ff @(posedge clk) begin
        if (we) begin
            // This creates complex nested Sel expressions
            for (int j = 0; j < 2; j++) begin
                if (be[j]) begin
                    memory[addr][j*4 +: 4] <= wdata[j*4 +: 4];
                end
            end
        end
    end
    
    // Test Issue 7: Packed struct assignment
    simple_packet_t packet_q, packet_d;
    
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            packet_q <= '0;
        end else begin
            packet_q <= packet_d;
        end
    end
    
    always_comb begin
        packet_d = packet_q;
        
        if (enable) begin
            // These assignments become Sel with constants
            packet_d.valid = 1'b1;
            packet_d.data = counter[3:0];
        end
    end
    
    // Test Issue 8: Generate block
    genvar i;
    generate
        for (i = 0; i < 2; i++) begin : gen_block
            logic [7:0] local_var;
            
            always_ff @(posedge clk) begin
                local_var <= memory[i];
            end
        end
    endgenerate
    
    // Test Issue 9: Simple assertion (will create Display/Sampled nodes)
    // synthesis translate_off
    assert property (@(posedge clk) disable iff (!rst_n)
        enable |-> state_q != STATE_A);
    // synthesis translate_on
    
    // Test Issue 10: Continuous assignment
    assign state_out = state_q;
    
    // Test Issue 11: Ternary in assignment
    logic [7:0] mux_out;
    assign mux_out = enable ? counter : 8'hFF;
    
    // Test Issue 12: Bit selection edge cases
    logic [15:0] wide_data;
    logic        bit_val;
    logic [3:0]  nibble;
    
    assign bit_val = wide_data[7];           // Single bit
    assign nibble = wide_data[7:4];          // Range
    assign comb_result = wide_data[addr[2:0] +: 8]; // Variable index with width

endmodule

// Test Issue 13: Simple testbench to create InitialStatic
module debug_tb;
    logic clk = 0;
    logic rst_n = 0;
    logic enable = 0;
    logic [7:0] result;
    logic [7:0] comb_result;
    debug_pkg::simple_state_t state_out;
    
    // Clock generation
    always #5 clk = ~clk;
    
    // Reset sequence - creates BLOCKTEMP
    initial begin
        rst_n = 0;
        repeat(3) @(posedge clk);
        rst_n = 1;
        enable = 1;
        repeat(10) @(posedge clk);
        $finish;
    end
    
    // DUT instantiation
    debug_test #(
        .WIDTH(8),
        .SIZE(4)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .enable(enable),
        .result(result),
        .comb_result(comb_result),
        .state_out(state_out)
    );
    
    // Test Issue 14: Display with formatting
    initial begin
        @(posedge rst_n);
        @(posedge clk);
        $display("Test started at time %0t", $time);
    end
    
endmodule
