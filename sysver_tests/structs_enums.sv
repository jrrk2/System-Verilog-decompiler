typedef struct packed {
    logic [7:0] addr;
    logic [7:0] data;
} packed_s;

typedef struct {
    int id;
    string name;
} unpacked_s;

typedef enum logic [1:0] {IDLE=0, RUN=1, DONE=2} state_e;

module struct_enum_test;
    packed_s ps;
    unpacked_s us;
    state_e st;
    initial begin
        ps = '{addr:8'hA5, data:8'h5A};
        us = '{id:1, name:"test"};
        st = RUN;
        $display("ps=%p us=%p st=%0d", ps, us, st);
    end
endmodule
