package main

import (
	"sort"
	"time"
)

type Block struct {
	cache *Cache
	index int
	lines []*Line
}

func NewBlock(cache *Cache, index int) *Block {
	block := new(Block)
	block.cache = cache
	block.index = index
	block.lines = make([]*Line, cache.linesPerBlock)
	return block
}

func (block *Block) access(address *Address) bool {
	for _, line := range block.lines {
		if line == nil {
			continue
		}

		if line.ContainsAddress(address) {
			line.lastAccessTime = time.Now()
			sort.Slice(block.lines, func(i, j int) bool {
				lineI := block.lines[i]
				if lineI == nil {
					return false
				}
				lineJ := block.lines[j]
				if lineJ == nil {
					return false
				}
				return lineI.lastAccessTime.Before(lineJ.lastAccessTime)
			})
			return true
		}
	}

	line := NewLine(block, address)
	block.lines = append(block.lines[1:], line)
	return false
}
