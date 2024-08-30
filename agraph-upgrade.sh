#!/bin/bash

# Script for pulling the AllegroGraph direct client fasl file of the
# version specified in the `agraph-config.sh`.

source agraph-config.sh

BASE_URL="https://franz.com/ftp/pri/acl/ag"
DIST_NAME="agraph-${AGRAPH_VERSION%.*}-linuxamd64.64smp-direct-client-lisp-acl10.1"
ARCHIVE_NAME="${DIST_NAME}.tar.gz"
FULL_URL="${BASE_URL}/ag${AGRAPH_VERSION}/linuxamd64.64smp/${ARCHIVE_NAME}"

wget "${FULL_URL}"
tar zxvf "${ARCHIVE_NAME}"
mv "${DIST_NAME}/agraph.fasl" ./agraph.fasl
rm -rf "${ARCHIVE_NAME}" "${DIST_NAME}"
