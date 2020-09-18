
from Crypto.PublicKey import RSA
import glob






t=glob.glob("/etc/ssl/certs/*.pem")


for x in t:
    try:
        # read the public key in:
        public_key = RSA.importKey(open(x, 'r').read())

        print public_key.n
        #print public_key.e

    except :
        pass

