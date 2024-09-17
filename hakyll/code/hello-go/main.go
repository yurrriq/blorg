package main

import (
	"fmt"
	"runtime"
)

var (
	Version    string
	CommitHash string
)

func main() {
	fmt.Printf("hello-go %s (sha=%s, goVersion=%s, platform=%s)\n",
		Version, CommitHash, runtime.Version(), runtime.GOOS+"/"+runtime.GOARCH)
}
