// Comprehensive SystemVerilog Test Case - All Common Synthesizable Constructs
// This module demonstrates most commonly encountered synthesizable SystemVerilog syntax

package test_pkg;
    // Package with types, constants, and functions
    
    // Enumerated types
    typedef enum logic [1:0] {
        IDLE   = 2'b00,
        ACTIVE = 2'b01,
        WAIT   = 2'b10,
        DONE   = 2'b11
    } state_t;
    
    // Packed structures
    typedef struct packed {
        logic        valid;
        logic [7:0]  data;
        logic [1:0]  mode;
        logic        error;
    } packet_t;
    
    // Unpacked structure
    typedef struct {
        int unsigned count;
        real         weight;
        string       name;
    } config_t;
    
    // Parameters
    localparam int WIDTH = 8;
    localparam logic [WIDTH-1:0] MASK = {WIDTH{1'b1}};
    
    // Function
    function automatic logic [WIDTH-1:0] reverse_bits(logic [WIDTH-1:0] data);
        for (int i = 0; i < WIDTH; i++) begin
            reverse_bits[i] = data[WIDTH-1-i];
        end
    endfunction
    
endpackage

module comprehensive_test import test_pkg::*; #(
    parameter int DATA_WIDTH = 16,
    parameter int ADDR_WIDTH = 8,
    parameter type data_t = logic [DATA_WIDTH-1:0],
    parameter data_t RESET_VALUE = '0
) (
    // Clock and reset
    input  logic clk_i,
    input  logic rst_ni,
    
    // Control signals
    input  logic        enable_i,
    input  logic        flush_i,
    output logic        ready_o,
    output logic        valid_o,
    
    // Data interface
    input  data_t       data_i,
    output data_t       data_o,
    input  logic        data_valid_i,
    output logic        data_ready_o,
    
    // Address interface
    input  logic [ADDR_WIDTH-1:0] addr_i,
    input  logic                  we_i,
    input  logic [DATA_WIDTH/8-1:0] be_i,
    
    // Status outputs
    output state_t      state_o,
    output packet_t     packet_o,
    output logic        error_o,
    output logic [7:0]  count_o
);

    // Internal signals and registers
    state_t state_q, state_d;
    data_t  data_reg_q, data_reg_d;
    logic   ready_q, ready_d;
    logic   valid_q, valid_d;
    logic   [7:0] counter_q, counter_d;
    logic   error_q, error_d;
    packet_t packet_q, packet_d;
    
    // Memory array
    data_t memory [2**ADDR_WIDTH];
    
    // Wire declarations
    logic load_enable;
    logic counter_enable;
    logic state_advance;
    data_t processed_data;
    logic [DATA_WIDTH-1:0] shifted_data;
    
    // Continuous assignments
    assign load_enable = enable_i && data_valid_i;
    assign counter_enable = state_q != IDLE;
    assign state_advance = counter_q == 8'hFF;
    assign data_ready_o = ready_q;
    
    // Generate block with for loop
    generate
        genvar i;
        for (i = 0; i < DATA_WIDTH/8; i++) begin : gen_byte_enables
            assign shifted_data[i*8 +: 8] = be_i[i] ? data_i[i*8 +: 8] : data_reg_q[i*8 +: 8];
        end
    endgenerate
    
    // Always_ff block for sequential logic
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_q    <= IDLE;
            data_reg_q <= RESET_VALUE;
            ready_q    <= 1'b0;
            valid_q    <= 1'b0;
            counter_q  <= 8'h00;
            error_q    <= 1'b0;
            packet_q   <= '0;
        end else begin
            state_q    <= state_d;
            data_reg_q <= data_reg_d;
            ready_q    <= ready_d;
            valid_q    <= valid_d;
            counter_q  <= counter_d;
            error_q    <= error_d;
            packet_q   <= packet_d;
        end
    end
    
    // Memory write
    always_ff @(posedge clk_i) begin
        if (we_i && enable_i) begin
            for (int j = 0; j < DATA_WIDTH/8; j++) begin
                if (be_i[j]) begin
                    memory[addr_i][j*8 +: 8] <= data_i[j*8 +: 8];
                end
            end
        end
    end
    
    // Combinational logic for next state and data processing
    always_comb begin
        // Default assignments
        state_d    = state_q;
        data_reg_d = data_reg_q;
        ready_d    = ready_q;
        valid_d    = 1'b0;
        counter_d  = counter_q;
        error_d    = error_q;
        packet_d   = packet_q;
        
        processed_data = data_reg_q;
        
        // State machine
        case (state_q)
            IDLE: begin
                ready_d = 1'b1;
                if (load_enable) begin
                    data_reg_d = shifted_data;
                    state_d = ACTIVE;
                    counter_d = 8'h00;
                    ready_d = 1'b0;
                end
            end
            
            ACTIVE: begin
                if (counter_enable) begin
                    counter_d = counter_q + 1'b1;
                    
                    // Data processing with various operators
                    unique case (counter_q[1:0])
                        2'b00: processed_data = data_reg_q << 1;
                        2'b01: processed_data = data_reg_q >> 1;
                        2'b10: processed_data = ~data_reg_q;
                        2'b11: processed_data = reverse_bits(data_reg_q[WIDTH-1:0]);
                    endcase
                    
                    data_reg_d = processed_data;
                    
                    // Conditional logic
                    if (state_advance) begin
                        state_d = WAIT;
                    end else if (flush_i) begin
                        state_d = IDLE;
                        error_d = 1'b1;
                    end
                end
            end
            
            WAIT: begin
                if (enable_i) begin
                    state_d = DONE;
                    valid_d = 1'b1;
                    
                    // Struct assignment
                    packet_d.valid = 1'b1;
                    packet_d.data = data_reg_q[7:0];
                    packet_d.mode = state_q;
                    packet_d.error = error_q;
                end
            end
            
            DONE: begin
                valid_d = 1'b1;
                if (flush_i) begin
                    state_d = IDLE;
                    counter_d = 8'h00;
                    error_d = 1'b0;
                    packet_d = '0;
                end
            end
            
            default: begin
                state_d = IDLE;
                error_d = 1'b1;
            end
        endcase
        
        // Priority encoder example
        if (error_q) begin
            state_d = IDLE;
        end else if (flush_i && (state_q != DONE)) begin
            state_d = IDLE;
        end
    end
    
    // Interface assignments with ternary operators
    assign ready_o = ready_q;
    assign valid_o = valid_q;
    assign data_o = (state_q == DONE) ? data_reg_q : memory[addr_i];
    assign state_o = state_q;
    assign packet_o = packet_q;
    assign error_o = error_q | (counter_q > 8'hF0);
    assign count_o = counter_q;
    
    // Assertions for verification (synthesis tools typically ignore these)
    // synthesis translate_off
    assert property (@(posedge clk_i) disable iff (!rst_ni)
        (state_q == DONE) |-> valid_o);
    
    assert property (@(posedge clk_i) disable iff (!rst_ni)
        (flush_i && (state_q != IDLE)) |-> (state_q == IDLE));
    // synthesis translate_on
    
    // Generate block with conditional generation
    generate
        if (DATA_WIDTH > 8) begin : gen_wide_data
            logic parity;
            always_comb begin
                parity = ^data_reg_q;  // XOR reduction
            end
        end else begin : gen_narrow_data
            // For narrow data widths, use simple parity
            // (This branch would be taken if DATA_WIDTH <= 8)
        end
    endgenerate
    
    // Instance of another module (if available)
    // submodule_example #(
    //     .WIDTH(DATA_WIDTH)
    // ) u_submodule (
    //     .clk_i(clk_i),
    //     .rst_ni(rst_ni),
    //     .data_i(data_reg_q),
    //     .data_o(/* connected to internal signal */)
    // );
    
endmodule

// Interface example
interface data_if #(parameter int WIDTH = 8) (input logic clk, input logic rst_n);
    logic [WIDTH-1:0] data;
    logic             valid;
    logic             ready;
    
    modport master (
        output data, valid,
        input  ready
    );
    
    modport slave (
        input  data, valid,
        output ready
    );
    
    // Interface functions
    function automatic logic is_transfer();
        return valid && ready;
    endfunction
    
endinterface

// Simple testbench structure
module testbench;
    logic clk = 0;
    logic rst_n = 0;
    
    // Clock generation
    always #5 clk = ~clk;
    
    // Reset generation  
    initial begin
        rst_n = 0;
        repeat(3) @(posedge clk);
        rst_n = 1;
    end
    
    // DUT instantiation would go here
    // comprehensive_test #(
    //     .DATA_WIDTH(16),
    //     .ADDR_WIDTH(8)
    // ) dut (
    //     .clk_i(clk),
    //     .rst_ni(rst_n),
    //     // ... other connections
    // );
    
endmodule
