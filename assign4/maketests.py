#!/usr/bin/python3
#
# Author:	Eric Power

"""
A script that creates tests (in racket) for Assignment 4 of CSC 330. Spring 2021 @ UVic. 
"""

# IMPORT STATEMENTS

# VARIABLES
list1 = [1,2,3,4,5,6,7,8,9,10]
list2 = [0,5,10,15,20]
list3 = [-1,-2,-3,-4,-5,-6]
list4 = [-2,10,15,20]
list5 = [-100, -200, 300, 400, 600]


# CONTROL FLOW
def main():

	pass


# MAKE TEST FUNCTIONS
def make_test_stream_for_n_steps(fnc_name, ans_list, n, name, fnc_name_short=""):
	"""Prints a test (in racket) for the functions that are tested using stream-for-n-steps.
	Requires the correct answer to be passed in through ans_list"""
	if fnc_name_short == "":
		fnc_name_short=fnc_name
	print("(check-equal? (stream-for-n-steps " + fnc_name + " " + str(n) + ") '" + 
		  get_rrl_string(ans_list) + ' "' + fnc_name_short + " test " + name + '")')


def make_test_add_pointwise_list(l, name):
	"""Prints a test (in racket) for add-pointwise-list"""
	new_list = l[0]
	for i in range(1, len(l)):
		new_list = add_pointwise(new_list, l[i])
	print("(check-equal? (add-pointwise-lists '" + get_rrl_string(l) +
		  " '" + get_rrl_string(new_list) + ' "add-pointwise-list test ' +
		  name + '"))' )


def make_test_add_pointwise(l1, l2, name):
	"""Prints a test (in racket) for add-pointwise"""
	answer = add_pointwise(l1, l2)
	print("(check-equal? (add-pointwise '", end="")
	print_rl(l1, end=" '")
	print_rl(l2, end=") ")
	print_rl(answer, end=" ")
	print(f'"add-pointwise test {name}")')


def add_pointwise(l1, l2):
	"""Adds two lists in pointwise manner"""
	answer = []
	ll = min(len(l1), len(l2))
	for i in range(ll):
		answer.append(l1[i] + l2[i])
	for i in range(ll, max(len(l1), len(l2))):
		if len(l1) > len(l2):
			answer.append(l1[i])
		elif len(l2) > len(l1):
			answer.append(l2[i])
	return answer


# HELPER FUNCTIONS
def fibo_term(n):
	"""Gets a list of the first n fibonacci numbers"""
	answer = []
	for i in range(n):
		if i == 0:
			answer.append(0)
		elif i == 1:
			answer.append(1)
		else:
			answer.append(answer[i-1] + answer[i-2])
	return answer


def get_rrl_string(l, constructor=""):
	"""get recursive racket list string"""
	answer = "(" + constructor
	for n, i in enumerate(l):
		if type(i) == int:
			answer += str(i)
		elif type(i) == list:
			answer += get_rrl_string(i)
		if n != len(l)-1:
			answer += " " 
	answer += ")"
	return answer


def print_rl(l, end="\n"):
	"""print racket list"""
	print("(", end="")
	for n, i in enumerate(l):
		print(i, end="")
		if n != len(l) - 1:
			print(" ", end="")
	print(")", end=end)
	


if __name__ == '__main__':
	main()


