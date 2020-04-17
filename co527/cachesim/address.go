package main

import (
	"strconv"
	"strings"
)

type Address struct {
	raw    string
	tag    uint64
	index  uint64
	offset uint64
}

func ParseAddress(raw string, cache *Cache) (*Address, error) {
	address := new(Address)
	address.raw = raw

	decimal, err := strconv.ParseUint(raw, 10, 64)
	if err != nil {
		return nil, err
	}

	binary := strconv.FormatUint(uint64(decimal), 2)
	if err != nil {
		return nil, err
	}
	binaryPadded := strings.Repeat("0", cache.bitsPerWord-len(binary)) + binary

	tagEnd := cache.bitsPerWord - (cache.blockOffset + cache.lineOffset)
	address.tag, err = strconv.ParseUint(binaryPadded[:tagEnd], 2, 64)
	if err != nil {
		return nil, err
	}
	indexEnd := tagEnd + cache.blockOffset
	if cache.blockOffset != 0 {
		address.index, err = strconv.ParseUint(binaryPadded[tagEnd:indexEnd], 2, 64)
		if err != nil {
			return nil, err
		}
	}
	offsetEnd := indexEnd + cache.lineOffset
	if cache.lineOffset != 0 {
		address.offset, err = strconv.ParseUint(binaryPadded[indexEnd:offsetEnd], 2, 64)
		if err != nil {
			return nil, err
		}
	}

	return address, nil
}
