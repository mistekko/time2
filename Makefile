PREFIX=${HOME}/.local

all: time2

time2: time2.lisp
	sbcl --disable-ldb \
	     --non-interactive \
	     --load time2.lisp\
	     --eval "(sb-ext:save-lisp-and-die \"time2\"\
		       :toplevel #'time2:main\
		       :executable t\
	               :save-runtime-options t)" \

install: time2
	mkdir -p ${PREFIX}/bin
	cp -f time2 ${PREFIX}/bin

clean:
	rm time2

force: clean
	make time2
