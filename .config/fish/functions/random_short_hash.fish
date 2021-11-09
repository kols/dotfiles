function random_short_hash
    PYENV_VERSION="2" pyenv exec python -c 'import os, hashlib; print hashlib.sha256(os.urandom(2048)).hexdigest()[:7]'
end
