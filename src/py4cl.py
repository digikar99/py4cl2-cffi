# This file is expected to be executed by RAW-PYEXEC after
# its contents are read into a string.
class LispCallbackObject (object):
	"""
	Represents a lisp function which can be called.

	An object is used rather than a lambda, so that the lifetime
	can be monitoried, and the function removed from a hash map
	"""
	lisp_callback_fn = getattr(py4cl_utils, "LispCallback_helper")
	lisp_callback_fn.restype = ctypes.py_object
	def __init__(self, handle):
		"""
		handle    A number, used to refer to the object in Lisp
		"""
		self.handle = handle
	def __call__(self, *args, **kwargs):
		if args is None: args = tuple()
		if kwargs is None: kwargs = dict()
		return LispCallbackObject.lisp_callback_fn(
		  ctypes.c_int(self.handle),
		  ctypes.py_object(args),
		  ctypes.py_object(kwargs)
		)

class UnknownLispObject (object):
	"""
	Represents an object in Lisp, which could not be converted to Python
	"""

	__during_init = True # Do not send changes during __init__

	def __init__(self, lisptype, handle):
		"""
		lisptype  A string describing the type. Mainly for debugging
		handle    A number, used to refer to the object in Lisp
		"""
		self.lisptype = lisptype
		self.handle = handle
		self.__during_init = False  # Further changes are sent to Lisp

	def __del__(self):
		"""
		Delete this object, sending a message to Lisp
		"""
		free_handle = getattr(py4cl_utils, "free_handle")
		free_handle(ctypes.c_int(self.handle))

	def __str__(self):
		return "UnknownLispObject(\"{0}\", {1})".format(self.lisptype, str(self.handle))

	def __getattr__(self, attr):
		# Check if there is a slot with this name
		try:
			sys.stdout = return_stream
			send_value("s", (self.handle, attr)) # Slot read
		finally:
			sys.stdout = output_stream

		# Wait for the result
		return message_dispatch_loop()

	def __setattr__(self, attr, value):
		if self.__during_init:
			return object.__setattr__(self, attr, value)
		try:
			sys.stdout = return_stream
			send_value("S", (self.handle, attr, value)) # Slot write
		finally:
			sys.stdout = output_stream
		# Wait until finished, to syncronise
		return message_dispatch_loop()

def generator(function, stop_value):
	temp = None
	while True:
		temp = function()
		if temp == stop_value: break
		yield temp

_py4cl_LispCallbackObject = LispCallbackObject
_py4cl_UnknownLispObject  = UnknownLispObject
_py4cl_generator = generator
