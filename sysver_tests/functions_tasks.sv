package func_pkg;
    function automatic int add(input int a, b);
        return a+b;
    endfunction

    task automatic print_val(input int v);
        $display("Value=%0d", v);
    endtask
endpackage

module func_task_test;
    import func_pkg::*;
    int res;
    initial begin
        res = add(2,3);
        print_val(res);
    end
endmodule
