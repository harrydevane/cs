package main

import (
	"math"
)

type Cache struct {
	bitsPerWord   int
	size          int
	bytesPerBlock int
	linesPerBlock int
	totalBlocks   int
	blockOffset   int
	lineOffset    int
	blocks        []*Block
}

func NewCache(bitsPerWord int, size int, bytesPerBlock int, linesPerBlock int) *Cache {
	cache := new(Cache)
	cache.bitsPerWord = bitsPerWord
	cache.size = size
	cache.bytesPerBlock = bytesPerBlock
	cache.linesPerBlock = linesPerBlock
	cache.totalBlocks = (size / bytesPerBlock)
	cache.blockOffset = int(math.Log2(float64(size / bytesPerBlock)))
	cache.lineOffset = int(math.Log2(float64(bytesPerBlock / linesPerBlock)))
	cache.blocks = make([]*Block, cache.totalBlocks)
	for i := 0; i < cache.totalBlocks; i++ {
		cache.blocks[i] = NewBlock(cache, i)
	}
	return cache
}

func (cache *Cache) access(address *Address) bool {
	block := cache.blocks[address.index]
	return block.access(address)
}
