SERVER = server
CLIENT = client

all :
	make -f Makefile-$(SERVER)
	make -f Makefile-$(CLIENT)

nc :
	make nc -f Makefile-$(SERVER)
	make nc -f Makefile-$(CLIENT)

clean :
	make clean -f Makefile-$(SERVER)
	make clean -f Makefile-$(CLIENT)
