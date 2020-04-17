package main

import (
	"time"
)

type Line struct {
	block          *Block
	address        *Address
	lastAccessTime time.Time
}

func NewLine(block *Block, address *Address) *Line {
	line := new(Line)
	line.block = block
	line.address = address
	line.lastAccessTime = time.Now()
	return line
}

func (line *Line) ContainsAddress(address *Address) bool {
	return line.address.tag == address.tag
}
