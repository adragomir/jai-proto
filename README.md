# jai-proto

Jai Protobuf module 

## Compile / run the generator

```
# in the generator dir
go build -o protoc-gen-jai main.go 

# running
PATH=$(pwd):$PATH protoc -I=/path/to/proto/dir --jai_out="/path/to/output/dir" /path/to/proto/dir/*.proto
```
