using PyCall

function call_n_times(n, fn)
    start = time()
    for i = 1:n
        x = fn(i)
    end
    return time() - start
end

function calls_per_second(n, fn)
    total_time = call_n_times(n, fn)
    per_call_time = total_time / n
    return 1/per_call_time
end

function pystr_i(i)
    return pystr(PyObject(i))
end

function pycall_str(i)
    return py"str"(PyObject(i))
end

calls_per_second(1, pystr_i)
print("Evaluating performance of pystr_i through 100000 calls...\n")
print("Calls per second: ", calls_per_second(100000, pystr_i), "\n\n")

calls_per_second(1, pycall_str)
print("Evaluating performance of pycall_str through 10000 calls...\n")
print("Calls per second: ", calls_per_second(10000, pycall_str), "\n")
