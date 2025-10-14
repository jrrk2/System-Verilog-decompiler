package func_pkg;
    function automatic logic [7:0] add(input logic [7:0] a, b);
        return a+b;
    endfunction

    task automatic not_val(input logic [7:0] v, output logic [7:0] vbar);
       vbar = !v;
    endtask
endpackage

module func_task_test(input logic [7:0] op1, op2, output logic [7:0] resbar);
    import func_pkg::*;
    logic [7:0] res;
    always @(op1 or op2) begin
        res = add(op1,op2);
        not_val(res, resbar);
    end
endmodule
