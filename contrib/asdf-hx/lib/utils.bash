#!/usr/bin/env bash

get_platform() {
  local platform
  platform=$(uname -s | tr '[:upper:]' '[:lower:]')

  case "$platform" in
    darwin)
      echo "apple-darwin"
      ;;
    linux)
      echo "unknown-linux-gnu"
      ;;
    mingw*|msys*|cygwin*)
      echo "pc-windows-msvc"
      ;;
    *)
      echo "Unsupported platform: $platform" >&2
      exit 1
      ;;
  esac
}

get_arch() {
  local arch
  arch=$(uname -m)

  case "$arch" in
    x86_64|amd64)
      echo "x86_64"
      ;;
    aarch64|arm64)
      echo "aarch64"
      ;;
    *)
      echo "Unsupported architecture: $arch" >&2
      exit 1
      ;;
  esac
}
