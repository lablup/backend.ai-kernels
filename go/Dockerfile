FROM golang:1.9.0-alpine3.6 as go-binary


FROM lablup/kernel-base-python3-minimal:latest

# Install Go environments
COPY --from=go-binary /usr/local/go /usr/local/
RUN apk add --no-cache ca-certificates

ENV GOPATH /home/work
ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" && chmod -R 777 "$GOPATH"
WORKDIR $GOPATH

# Install kernel-runner scripts package 
# TODO: use PyPI's package if available
RUN apk add --no-cache git
RUN cd /home/sorna && \
    git clone https://github.com/lablup/backend.ai-kernel-runner.git
RUN cd /home/sorna/backend.ai-kernel-runner && pip install -e .[golang]
# RUN pip install backend.ai-kernel-runner[golang]

COPY policy.yml /home/sorna/

LABEL io.sorna.features "batch query uid-match user-input"

CMD ["/home/sorna/jail", "-policy", "/home/sorna/policy.yml", \
     "/usr/local/bin/python", "-m", "ai.backend.kernel", "golang"]
