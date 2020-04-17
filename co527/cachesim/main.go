package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	files, err := ioutil.ReadDir("./input")
	if err != nil {
		fmt.Println("Error while reading input files", err)
		return
	}
	for _, fileInfo := range files {
		name := fileInfo.Name()
		fmt.Println(name)

		file, err := os.Open("./input/" + name)
		if err != nil {
			fmt.Println("Error while reading file", err)
			return
		}
		defer file.Close()

		scanner := bufio.NewScanner(file)
		var cache *Cache
		for scanner.Scan() {
			line := scanner.Text()
			if cache == nil {
				split := strings.Split(line, " ")
				bitsPerWord, err := strconv.Atoi(split[0])
				if err != nil {
					fmt.Println("Error while parsing description", err)
					return
				}
				size, err := strconv.Atoi(split[1])
				if err != nil {
					fmt.Println("Error while parsing description", err)
					return
				}
				bytesPerBlock, err := strconv.Atoi(split[2])
				if err != nil {
					fmt.Println("Error while parsing description", err)
					return
				}
				linesPerBlock, err := strconv.Atoi(split[3])
				if err != nil {
					fmt.Println("Error while parsing description", err)
					return
				}
				cache = NewCache(bitsPerWord, size, bytesPerBlock, linesPerBlock)
				continue
			}

			address, err := ParseAddress(line, cache)
			if err != nil {
				fmt.Println("Error while parsing address", err)
				return
			}
			if cache.access(address) {
				fmt.Print("C")
			} else {
				fmt.Print("M")
			}
		}
		fmt.Println("")
	}
}
