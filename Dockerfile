FROM bitnami/minideb:bookworm

RUN install_packages ca-certificates cmake curl g++ gcc git gnat gprbuild less libc6-dev libgsl-dev libgnatcoll21-dev make nim unzip wget xz-utils
RUN (curl -fsSL https://deno.land/install.sh | DENO_INSTALL=/usr/local sh)
RUN (wget https://github.com/gleam-lang/gleam/releases/download/v1.6.2/gleam-v1.6.2-x86_64-unknown-linux-musl.tar.gz -O - | tar xz) && mv gleam /usr/local/bin
RUN (wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz -O - | tar xJ) && mv zig-linux-x86_64-0.11.0 /usr/local/zig && ln -sf /usr/local/zig/zig /usr/local/bin/zig
RUN (wget https://go.dev/dl/go1.25.4.linux-amd64.tar.gz -O - | tar xz) && mv go /usr/local/go && ln -sf /usr/local/go/bin/go /usr/local/bin/go
