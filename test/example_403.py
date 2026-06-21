from cryptography.hazmat.primitives import hashes

alg = hashes.SHA256()
digest = hashes.Hash(alg)
digest.update(b"abc")
print(isinstance(alg, hashes.HashAlgorithm))
print(digest.finalize()[:2] == b"\xba\x78")
