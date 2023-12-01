FROM bitnami/minideb:latest

RUN \
  install_packages ca-certificates gcc git gnat gprbuild haxe less libc6-dev libgsl-dev libgnatcoll21-dev make nim python3 wget xz-utils && \
  (wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz -O - | tar xJ) && mv zig-linux-x86_64-0.11.0 /usr/local/zig && ln -sf /usr/local/zig/zig /usr/local/bin/zig
