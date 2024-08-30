#!/bin/bash

source agraph-config.sh

docker run -d \
            --name=brdf-agraph \
            -v $AGRAPH_DATA:/agraph/data \
            -e AGRAPH_SUPER_USER \
            -e AGRAPH_SUPER_PASSWORD \
            -e AGRAPH_LICENSE \
            -p 10100-10136:10000-10036 \
            --shm-size 2g \
	    franzinc/agraph:v$AGRAPH_VERSION
