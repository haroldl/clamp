from cryptography.hazmat.primitives import hashes

alg = hashes.SHA256()
print(hashes.__name__)
print(alg.name)
print(alg.digest_size)
