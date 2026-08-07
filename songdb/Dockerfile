FROM debian:13-slim

SHELL ["/bin/bash", "-c"]

ENV PATH="$PATH:/root/.cache/scalacli/local-repo/bin/scala-cli" \
    LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"

RUN apt -y update && \
    apt -y install curl wget ca-certificates git libchromaprint-tools sox zstd && \
    apt -y clean

RUN curl -sSLf https://scala-cli.virtuslab.org/get | sh

# fail early if scala-cli is not installed where expected
RUN scala-cli

RUN git clone --depth=1 --single-branch https://github.com/mvtiaine/audacious-uade-tools

RUN cd audacious-uade-tools && \
    mkdir -p songdb/sources/audio && \
    cd songdb/sources/audio && \
    rm -f audio_*.zst && \
    for i in 0 1 2 3 4 5 6 7 8 9 a b c d e f; do wget --no-verbose https://github.com/mvtiaine/audacious-uade-tools/releases/download/audio/audio_$i.tsv.zst; done && \
    zstd -d -f --rm audio_*.zst && \
    cd ../.. && \
    $(./audio_match.sc || true) && \
    $(./find_dupes.sc || true)

WORKDIR /audacious-uade-tools/songdb

# Build image:
# docker build . -t audacious-uade-tools
# docker build . --no-cache -t audacious-uade-tools

# Example usages:
# docker run --rm audacious-uade-tools ./audio_match.sc AQAAC1EShUokRcMfoT-OX8RfNKHCG5V6iEue48cdHQAEEgYRCQhA0AAD
# cat somefile.wav | docker run -i --rm audacious-uade-tools /bin/bash -c "fpcalc -plain - | ./audio_match.sc -"
# cat somefile.mod | docker run -i --rm audacious-uade-tools ./find_dupes.sc -

# Record and match using microphone:
# (ctrl-c to stop recording, record at least 30+s, SoX is needed on the host for rec command)
# rec -r 11025 -c 1 -t wav - | docker run -i --rm audacious-uade-tools /bin/bash -c "fpcalc -plain - | ./audio_match.sc -"
