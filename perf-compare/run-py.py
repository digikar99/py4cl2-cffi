import time

def call_n_times(n, fn):
	start_time = time.process_time()
	for i in range(n): fn(i)
	return time.process_time() - start_time

def calls_per_second (n, fn):
	total_time = call_n_times(n, fn)
	per_call_time = total_time / n
	return 1/per_call_time

def pystr(i):
	return str(i)

def pycall_str(i):
	return globals()['__builtins__'].__dict__['str'](i)

calls_per_second(1, pystr)
print("Evaluating performance of pystr_i through 1000000 calls...")
print("Calls per second: ", calls_per_second(1000000, pystr), "\n")

calls_per_second(1, pycall_str)
print("Evaluating performance of pycall_str through 1000000 calls...")
print("Calls per second: ", calls_per_second(1000000, pycall_str),)
