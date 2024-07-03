#!/usr/bin/bash
HERE=`readlink -f .`
SUFFIX=${HERE////-} # replace / with -
IMG_SUFFIX=${SUFFIX,,} # to lowercase
ctr=$(buildah from ubuntu:24.04);
IMG=erlang-image$IMG_SUFFIX

buildah run "$ctr" -- sh -c "echo 'Australia/Perth' > /etc/timezone"
buildah run "$ctr" -- sh -c "cat > /etc/apt/sources.list << EOF
deb mirror://mirrors.ubuntu.com/mirrors.txt noble main restricted universe multiverse
deb mirror://mirrors.ubuntu.com/mirrors.txt noble-updates main restricted universe multiverse
deb mirror://mirrors.ubuntu.com/mirrors.txt noble-backports main restricted universe multiverse
deb mirror://mirrors.ubuntu.com/mirrors.txt noble-security main restricted universe multiverse
EOF"
buildah run "$ctr" -- rm /etc/apt/sources.list.d/ubuntu.sources
buildah run "$ctr" -- apt update

buildah run "$ctr" -- apt install --assume-yes erlang-nox rebar3 python3-venv

buildah add "$ctr" ./requirements.txt /tmp/
buildah run "$ctr" -- python3 -m venv /var/www/venv
buildah run "$ctr" -- /var/www/venv/bin/pip install -r /tmp/requirements.txt

buildah commit --rm "$ctr" "$IMG"

echo $IMG

WORKING_CTR=`buildah from $IMG`
buildah run -v $HERE:/var/www/gitchat --env PYTHON_BIN=/var/www/venv/bin/python --workingdir /var/www/gitchat $WORKING_CTR -- rebar3 shell

# copy host folder to container
# buildah add "$ctr" . /var/www/gitchat

# Dev essentials
# buildah run "$ctr" -- apt install --assume-yes iproute2 make git build-essential curl erlang-dev vim
# buildah commit "$ctr" "erlang-image-dev"
