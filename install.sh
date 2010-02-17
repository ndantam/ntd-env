#!/bin/bash

find ntd -type d | sed -e 's!^ntd/*!!' | xargs -I '{}' mkdir -pv ~/'{}'

find ntd -type f | sed -e 's!^ntd/*!!' | xargs -I '{}' mv -vi  ~/'{}' ~/'{}'.bak

find ntd -type f | sed -e 's!^ntd/*!!' | xargs -I '{}' ln -v ntd/'{}'  ~/'{}'
