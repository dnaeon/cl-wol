FROM clfoundation/sbcl:2.1.7 as builder

ENV QUICKLISP_ADD_TO_INIT_FILE=true

WORKDIR /root/quicklisp/local-projects/cl-wol
COPY . .
RUN /usr/local/bin/install-quicklisp && make cli

FROM debian:bullseye-slim
WORKDIR /app
COPY --from=builder /root/quicklisp/local-projects/cl-wol/bin/wol .
ENTRYPOINT ["./wol"]
