
from Crypto.PublicKey import RSA

# read the public key in:
public_key = RSA.importKey(open('key.pub.pub', 'r').read())

print public_key.n
print public_key.e
