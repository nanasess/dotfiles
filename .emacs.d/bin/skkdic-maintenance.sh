#!/usr/bin/env bash

CURRENT_DIR=$(cd $(dirname $0); pwd)
ONEDRIVE_DIR="${HOME}/OneDrive - Skirnir Inc/emacs/ddskk"

mkdir -p skk-jisyo
cd skk-jisyo

# https://skk-dev.github.io/dict/
while read -r line; do
    echo "Downloading $line"
    curl -O "https://skk-dev.github.io/dict/${line}.gz"
    gunzip -d "${line}.gz"
    iconv -f euc-jisx0213 -t utf8 "${line}" \
        | sed -e 's/coding: euc-jp/coding: utf-8/g' \
        | sed -e 's/coding: euc-jis-2004/coding: utf-8/g' \
              > "${line}.utf8"
    cp -p "${line}.utf8" "${ONEDRIVE_DIR}/${line}.utf8"
done < <(cat <<EOF
SKK-JISYO.JIS2
SKK-JISYO.JIS2004
SKK-JISYO.JIS3_4
SKK-JISYO.L
SKK-JISYO.assoc
SKK-JISYO.fullname
SKK-JISYO.geo
SKK-JISYO.itaiji
SKK-JISYO.jinmei
SKK-JISYO.law
SKK-JISYO.lisp
SKK-JISYO.mazegaki
SKK-JISYO.okinawa
SKK-JISYO.propernoun
SKK-JISYO.pubdic+
SKK-JISYO.station
EOF
)

while read -r line; do
    echo "Downloading $line"
    curl -O "https://skk-dev.github.io/dict/$line"
    tar xf "$line"
done < <(cat <<EOF
SKK-JISYO.edict.tar.gz
zipcode.tar.gz
EOF
)

iconv -f euc-jisx0213 -t utf8 SKK-JISYO.edict \
    | sed -e 's/coding: euc-jp/coding: utf-8/g' \
          > SKK-JISYO.edict.utf8
iconv -f euc-jisx0213 -t utf8 zipcode/SKK-JISYO.zipcode \
    | sed -e 's/coding: euc-jis-2004/coding: utf-8/g' \
          > SKK-JISYO.zipcode.utf8
iconv -f euc-jisx0213 -t utf8 zipcode/SKK-JISYO.office.zipcode \
    | sed -e 's/coding: euc-jis-2004/coding: utf-8/g' \
          > SKK-JISYO.office.zipcode.utf8
cp -rp SKK-JISYO.edict.utf8 "${ONEDRIVE_DIR}/SKK-JISYO.edict.utf8"
cp -rp SKK-JISYO.zipcode.utf8 "${ONEDRIVE_DIR}/SKK-JISYO.zipcode.utf8"
cp -rp "${ONEDRIVE_DIR}/SKK-JISYO.office.zipcode.utf8" SKK-JISYO.office.zipcode.utf8

curl -O https://raw.githubusercontent.com/uasi/skk-emoji-jisyo/master/SKK-JISYO.emoji.utf8
cp -rp SKK-JISYO.emoji.utf8 "${ONEDRIVE_DIR}/SKK-JISYO.emoji.utf8"

curl -sS https://raw.githubusercontent.com/ymrl/SKK-JISYO.emoji-ja/master/SKK-JISYO.emoji-ja.utf8 | skkdic-sort > SKK-JISYO.emoji-ja.utf8
cp -rp SKK-JISYO.emoji-ja.utf8 "${ONEDRIVE_DIR}/SKK-JISYO.emoji-ja.utf8"

curl -sS https://raw.githubusercontent.com/skk-dev/dict/master/SKK-JISYO.emoji > SKK-JISYO.emoji_official.utf8
# see https://github.com/eidera/skktools/blob/master/scripts/run.bash

skkdic-expr2 \
    SKK-JISYO.L + \
    SKK-JISYO.assoc + \
    SKK-JISYO.edict + \
    SKK-JISYO.fullname + \
    SKK-JISYO.geo + \
    SKK-JISYO.jinmei + \
    SKK-JISYO.law + \
    SKK-JISYO.propernoun + \
    SKK-JISYO.station + \
    zipcode/SKK-JISYO.zipcode + \
    zipcode/SKK-JISYO.office.zipcode + \
    SKK-JISYO.JIS2 + \
    SKK-JISYO.JIS2004 + \
    SKK-JISYO.JIS3_4 \
    > SKK-JISYO.all.euc-jisx0213

skkdic-expr2 \
    SKK-JISYO.L.utf8 + \
    SKK-JISYO.assoc.utf8 + \
    SKK-JISYO.edict.utf8 + \
    SKK-JISYO.fullname.utf8 + \
    SKK-JISYO.geo.utf8 + \
    SKK-JISYO.jinmei.utf8 + \
    SKK-JISYO.law.utf8 + \
    SKK-JISYO.propernoun.utf8 + \
    SKK-JISYO.station.utf8 + \
    SKK-JISYO.zipcode.utf8 + \
    SKK-JISYO.office.zipcode.utf8 + \
    SKK-JISYO.JIS2.utf8 + \
    SKK-JISYO.JIS2004.utf8 + \
    SKK-JISYO.JIS3_4.utf8 + \
    SKK-JISYO.emoji.utf8 + \
    SKK-JISYO.emoji-ja.utf8 + \
    SKK-JISYO.emoji_official.utf8 \
    > SKK-JISYO.all.utf8

cd $CURRENT_DIR
cp -rp skk-jisyo/SKK-JISYO.all.utf8 "${ONEDRIVE_DIR}/SKK-JISYO.all.utf8"
# backword compatibility
cp -rp skk-jisyo/SKK-JISYO.all.euc-jisx0213 "${ONEDRIVE_DIR}/SKK-JISYO.ALL.nosort"
cp -rp skk-jisyo/SKK-JISYO.all.euc-jisx0213 "${ONEDRIVE_DIR}/SKK-JISYO.ALL"
rm -rf skk-jisyo
