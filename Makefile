PREFIX=${HOME}/.local

all: comprof

comprof: comprof.lisp
	sbcl --disable-ldb \
	     --non-interactive \
	     --load comprof.lisp\
	     --eval "(sb-ext:save-lisp-and-die \"comprof\"\
		       :toplevel #'comprof:main\
		       :executable t\
	               :save-runtime-options t)" \

install: comprof
	mkdir -p ${PREFIX}/bin
	cp -f comprof ${PREFIX}/bin

clean:
	rm comprof

force: clean
	make comprof
