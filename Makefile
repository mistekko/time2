PREFIX=${HOME}/.local
LISP=sbcl

all: comprof

comprof: comprof.lisp
	${LISP} --load comprof.lisp\
		--eval "(sb-ext:save-lisp-and-die \"comprof\"\
						  :toplevel #'main\
						  :executable t)"

install:
	mkdir -p ${PREFIX}/bin
	cp -f comprof ${PREFIX}/bin

clean:
	rm comprof
