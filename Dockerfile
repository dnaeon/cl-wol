FROM clfoundation/sbcl:2.2.4 as builder

ENV QUICKLISP_ADD_TO_INIT_FILE=true
ENV QUICKLISP_DIST_VERSION=latest

WORKDIR /app
COPY . .
RUN /usr/local/bin/install-quicklisp && make cli

FROM debian:bullseye-slim
WORKDIR /app
COPY --from=builder /app/bin/wol .
ENTRYPOINT ["./wol"]
